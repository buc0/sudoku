#!/usr/bin/perl

use strict;
use warnings;

use Data::Dumper;

$Data::Dumper::Indent = 1;

my @rows;
my @columns;
my @blocks;

{
    package Sudoku::Controller;

    my $stage = 'fact-setting';
    my %pending_facts;
    my %pending_inferences;

    sub fact_setting { $stage eq 'fact-setting' };
    sub inferring { $stage eq 'inferring' };

    # this should be a function that "makes it so"
    sub add_fact {
        my $key = shift;
        my $callback = shift;
        $pending_facts{$key} = $callback;
    }

    # this should be a function that "makes it so"
    sub add_inference {
        my $key = shift;
        my $callback = shift;
        $pending_inferences{$key} = $callback;
    }

    sub _find_shared_group {
        my( $cell_a, $cell_b ) = @_;

        my @a_groups = @{$cell_a->{_GROUPS}};

        die unless @a_groups == 3;

        my @b_groups = @{$cell_b->{_GROUPS}};

        die unless @b_groups == 3;

        my @shared_groups = grep { $a_groups[0] == $_ || $a_groups[1] == $_ || $a_groups[2] == $_ } @b_groups;

        return @shared_groups;
    }

    sub _group_type {
        my $group = shift or die;

        my $group_id = $group->{_ID};

        $group_id =~ s/ .*//;

        return $group_id;
    }

    sub inference_loop {
        die unless fact_setting;

        while( keys %pending_inferences ) {
            $stage = 'inferring';

            my $message = "moved from fact-setting to inferring with ";

            while( keys %pending_inferences ) {
                my @inferences = values %pending_inferences;
                %pending_inferences = ();

                print $message . @inferences . " pending inferences\n";

                foreach my $inference( @inferences ) {
                    $inference->();
                }

                $message = "while inferring, added ";
            }

            # to next level of inference:
            #   + an "either-or" pair is a pair of cells in a group that where one or the other MUST have a specific value
            #   + each cell can be a part of up to three 'either-or' (e-o) pairs for the same number
            #   + when a cell is in multiple e-o pairs for the same number, those pairs are 'linked' into a list
            #   + a list of linked e-o pairs can form a closed loop (cycle)
            #   + if a cycle is formed with an odd number of pairs, this is an impossible cycle
            #   + if two members of a list share a group but don't directly form an e-o pair, this is a weak link
            #   + if a weak linkage would create an impossible cycle, NEITHER cell in the weak linkage has that value
            #    + otherwise the weak link can be promoted to a strong link
            #         (meaning that no other cell in the group can have that value)
            #   + if two members of different lists share a group but don't directly from an e-o pair, this is a tenative link
            #   + (speculative) if two lists are connected by multiple tentative links, they should be merged into one list
            #       and the tentative links upgraded to weak links
            my %pairs_by_n;
            foreach my $group( @rows, @columns, @blocks ) {
                my $group_type = _group_type( $group );
                foreach my $pair( $group->get_either_or_pairs() ) {
                    push @{$pairs_by_n{ $pair->{n} }}, { group_type => $group_type, pair => $pair->{pair} };
                }
            }

            foreach my $n( 1 .. 9 ) {
                next unless exists $pairs_by_n{$n};

                my @unlinked = @{$pairs_by_n{$n}};

                UNLINKED: while( @unlinked ) {
                    my $first = shift @unlinked;

                    last unless @unlinked;

                    # build the initial link structure, linking the two cells via their common group
                    my @first_pair = @{ $first->{pair} };

                    my %list;

                    $list{ $first_pair[0] } = { cell => $first_pair[0], $first->{group_type} => { type => 'strong', cell => $first_pair[1] } };
                    $list{ $first_pair[1] } = { cell => $first_pair[1], $first->{group_type} => { type => 'strong', cell => $first_pair[0] } };

                    my @newly_added_cells;

                    push @newly_added_cells, @first_pair;

                    # while I have newly-added cells in this link, look through the rest of @unlinked for those cells, then add them
                    while( @newly_added_cells ) {
                        my $new_cell = shift @newly_added_cells;

                        for( my $i = $#unlinked; $i >= 0; $i-- ) {
                            if( $unlinked[$i]{pair}[0] == $new_cell or $unlinked[$i]{pair}[1] == $new_cell ) {
                                my( $new_pair ) = splice @unlinked, $i, 1;

                                my( $newest_cell ) = grep { $_ != $new_cell } @{$new_pair->{pair}};

                                $list{$new_cell}{ $new_pair->{group_type} } = { type => 'strong', cell => $newest_cell };

                                if( not exists $list{$newest_cell} ) {
                                    push @newly_added_cells, $newest_cell;
                                    $list{$newest_cell} = { cell => $newest_cell };
                                }

                                $list{$newest_cell}{ $new_pair->{group_type} } = { type => 'strong', cell => $new_cell };
                            }
                        }
                    }

                    # look for weak links between cells
                    my @cells = map { $list{$_}{cell} } keys %list;

                    while( @cells ) {
                        my $cur_cell = shift @cells;

                        foreach my $other_cell( @cells ) {
                            my @shared_groups = _find_shared_group( $cur_cell, $other_cell );

                            foreach my $shared_group( @shared_groups ) {
                                my $group_type = _group_type( $shared_group );

                                next if( $list{$cur_cell}{$group_type} and $list{$cur_cell}{$group_type}{cell} == $other_cell );

                                if( $list{$cur_cell}{$group_type} ) {
                                    # we just found a cycle of length 3, all within one block
                                    # the pair of cells that share only the block are the ones
                                    # that cannot be $n

                                    my $c3 = $list{$cur_cell}{$group_type}{cell};

                                    if( @shared_groups == 1 ) {
                                        $cur_cell->not_possible( $n, "would create dependency cycle of length 3" );
                                        $other_cell->not_possible( $n, "would create dependency cycle of length 3" );
                                    }
                                    elsif( _find_shared_group( $cur_cell, $c3 ) == 1 ) {
                                        $cur_cell->not_possible( $n, "would create dependency cycle of length 3" );
                                        $c3->not_possible( $n, "would create dependency cycle of length 3" );
                                    }
                                    else {
                                        $other_cell->not_possible( $n, "would create dependency cycle of length 3" );
                                        $c3->not_possible( $n, "would create dependency cycle of length 3" );
                                    }

                                    next UNLINKED;
                                }

                                $list{$cur_cell}{$group_type} = { type => 'weak', cell => $other_cell };
                                $list{$other_cell}{$group_type} = { type => 'weak', cell => $cur_cell };
                            }
                        }
                    }

                    my @cycles;

                    # look for cycles
                    my( $start_cell ) = sort map { $list{$_}{cell} } keys %list;

                    my @paths;
                    my @agenda = [ $start_cell, [] ];

                    while( @agenda ) {
                        my $next_item = shift @agenda;
                        my $cur_cell = $next_item->[0];
                        my @parents = @{ $next_item->[1] };

                        foreach my $group_type( qw(row column block) ) {
                            if( my $link = $list{$cur_cell}{$group_type} ) {
                                my $linked_cell = $link->{cell};

                                if( @parents and $linked_cell == $parents[-1] ) {
                                    # going right back to whence we came, ignore it
                                }
                                elsif( grep { $_ == $linked_cell } @parents ) {
                                    my @cycle = ( $linked_cell, $cur_cell );

                                    my $parent_index = $#parents;
                                    while( $parents[$parent_index] != $linked_cell ) {
                                        push @cycle, $parents[ $parent_index-- ];
                                    }

                                    push @cycles, \@cycle;
                                }
                                else {
                                    push @agenda, [ $linked_cell, [ @parents, $cur_cell ] ];
                                }
                            }
                        }
                    }

                    # keep only odd cycles (supposed to be impossible w/o a weak link)
                    @cycles = grep { @$_ % 2 } @cycles;

                    # canonicalize the cycles and eliminate duplicates
                    # canonicalization is selecting the "lowest" cell (by
                    # numerical comparison of the references) to be the
                    # arbitrary start and then selecting a direction such
                    # that the next cell is the lower (again by numerical
                    # comparison of the references) of the two adjacent
                    # cells
                    my %cycles;

                    foreach my $cycle( @cycles ) {
                        my( $cycle_start_cell ) = sort @$cycle;

                        while( $cycle->[0] != $cycle_start_cell ) {
                            push @$cycle, shift @$cycle;
                        }

                        if( $cycle->[-1] < $cycle->[1] ) {
                            push @$cycle, shift @$cycle;
                            @$cycle = reverse @$cycle;
                        }

                        $cycles{"@$cycle"} = $cycle;
                    }

                    foreach my $cycle( values %cycles ) {
                        my @weak_links;

                        for( my $i = 0; $i < @$cycle; $i++ ) {
                            my $cell_info = $list{ $cycle->[$i] };

                            # it is possible for adjacent cells to be linked two times, but those links will both be of the same type
                            my( $backwards_link ) = grep { $_ and $_->{cell} == $cycle->[ $i - 1 ] } @$cell_info{ qw(row column block) };

                            die unless $backwards_link;

                            if( $backwards_link->{type} eq 'weak' ) {
                                push @weak_links, [ $cycle->[$i], $cycle->[ $i - 1 ] ];
                            }
                        }

                        if( not @weak_links ) {
                            die "impossible odd-length cycle";
                        }
                        elsif( @weak_links == 1 ) {
                            # we know that the cells involved in the weak link do NOT have the value $n...
                            $weak_links[0][0]->not_possible( $n, "would create odd length dependency cycle" );
                            $weak_links[0][1]->not_possible( $n, "would create odd length dependency cycle" );
                        }
                    }
                }
            }

            $stage = 'fact-setting';

            my @new_facts = values %pending_facts;
            %pending_facts = ();

            print "moved from inferring to fact-setting with " . @new_facts . " new facts\n";

            foreach my $fact( @new_facts ) {
                print "fact: ";
                $fact->();
            }

            die if keys %pending_facts;
        }
    }
}

{
    package Sudoku::Cell;

    use strict;
    use warnings;

    sub new {
        my( $proto, $args ) = @_;
        my $class = ref $proto || $proto;

        my $self = {
            _ID => $args->{id},
            _VALUE => undef,
            _GROUPS => [],
            _POSSIBLE => { map { ( $_ => 1 ) } 1 .. 9 },
        };

        bless $self, $class;
    }

    sub value {
        my $self = shift;

        if( @_ ) {
            my( $newval, $reason) = @_;

            $reason ||= '';

            return if defined $self->{_VALUE} and $self->{_VALUE} == $newval;

            die "conflicting values $self->{_ID} = $self->{_VALUE} (told = $newval, $reason)" if defined $self->{_VALUE} and $self->{_VALUE} != $newval;

            die "conflicting valuse $self->{_ID} != $newval (told =, $reason)" unless $self->{_POSSIBLE}{$newval};

            if( Sudoku::Controller::fact_setting() ) {

                print "setting cell $self->{_ID} to $newval ($reason)\n";

                $self->{_POSSIBLE} = { 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0, 9 => 0, $newval => 1 };
                $self->{_VALUE} = $newval;

                foreach my $group( @{$self->{_GROUPS}} ) {
                    Sudoku::Controller::add_inference( "$self->{_ID}=$newval($group)", sub { $group->cell_set( { cell => $self, value => $newval } ) } );
                }
            }
            elsif( Sudoku::Controller::inferring() ) {
                Sudoku::Controller::add_fact( "$self->{_ID}=$newval", sub { $self->value( $newval, $reason ); } );
            }
            else {
                die;
            }
        }

        return $self->{_VALUE};
    }

    sub add_group {
        my( $self, $group ) = @_;

        push @{$self->{_GROUPS}}, $group;
    }

    sub not_possible {
        my( $self, $val, $reason ) = @_;

        $reason ||= '';

        die unless defined $val;

        die "conflict $self->{_ID} = $self->{_VALUE} (told !=, $reason)" if defined $self->{_VALUE} and $val == $self->{_VALUE};

        return if defined $self->{_VALUE};

        return unless $self->{_POSSIBLE}{$val};

        if( Sudoku::Controller::fact_setting() ) {
            print "marking $val as not possible for cell $self->{_ID} ($reason)\n";

            $self->{_POSSIBLE}{$val} = 0;

            my @possible = grep { $self->{_POSSIBLE}{$_} } 1 .. 9;

            if( @possible == 0 ) {
                die "nothing possible";
            }
            elsif( @possible == 1 ) {
                Sudoku::Controller::add_inference( "$self->{_ID}=$possible[0]", sub { $self->value( $possible[0], "only value left in cell" ) } );
            }
            else {
                foreach my $group( @{$self->{_GROUPS}} ) {
                    Sudoku::Controller::add_inference( "$self->{_ID}!=$val($group)", sub { $group->cell_not( { cell => $self, value => $val } ) } );
                }
            }
        }
        elsif( Sudoku::Controller::inferring() ) {
            Sudoku::Controller::add_fact( "$self->{_ID}!=$val", sub { $self->not_possible( $val, $reason ) } );
        }
        else {
            die;
        }
    }

    sub get_state {
        my( $self ) = @_;

        if( defined $self->{_VALUE} ) {
            return '    ' . $self->{_VALUE} . "    ";
        }
        else {
            return join( '', map { $self->{_POSSIBLE}{$_} ? ' ' : '.' } 1 .. 9 );
        }
    }

    sub get_possible {
        my( $self ) = @_;

        if( defined $self->{_VALUE} ) {
            return $self->{_VALUE};
        }
        else {
            return grep { $self->{_POSSIBLE}{$_} } 1 .. 9;
        }
    }
}

{
    package Sudoku::Group;

    use strict;
    use warnings;

    sub new {
        my( $proto, $args ) = @_;
        my $class = ref $proto || $proto;

        my $self = {
            _ID => $args->{id},
            _CELLS => [],
            _POSSIBLE => { map { ( $_ => {} ) } 1 .. 9 },
            _SETS => [],
        };

        bless $self, $class;
    }

    sub add_cell {
        my $self = shift;

        my( $args ) = @_;

        die "duplicate $args->{nbr} in $self->{_ID}" if defined $self->{_CELLS}[$args->{nbr}];

        $self->{_CELLS}[$args->{nbr}] = $args->{cell};

        for my $i( 1 .. 9 ) {
            $self->{_POSSIBLE}{$i}{$args->{nbr}} = 1;
        }

        $args->{cell}->add_group( $self );
    }

    sub _find_cell {
        my( $self, $cell ) = @_;

        my $cell_index;

        for my $i( 0 .. 8 ) {
            if( $self->{_CELLS}[$i] == $cell ) {
                $cell_index = $i;
                last;
            }
        }

        die "cell not found" unless defined $cell_index;

        return $cell_index;
    }

    sub cell_set {
        my( $self, $args ) = @_;

        die unless $args->{cell};

        die if Sudoku::Controller::fact_setting();

        my $cell_index = $self->_find_cell( $args->{cell} );

        $self->{_POSSIBLE}{$args->{value}} = { 0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0, 9 => 0, $cell_index => 1 };

        for my $i( 0 .. 8 ) {
            if( ( $i + 1 ) != $args->{value} ) {
                $self->{_POSSIBLE}{$i + 1}{$cell_index} = 0;
            }

            next if $i == $cell_index;

            $self->{_CELLS}[$i]->not_possible( $args->{value}, "$args->{cell}{_ID} set to $args->{value}" );
        }

        Sudoku::Controller::add_inference( "$self->{_ID}", sub { $self->_check_remaining; } );
    }

    sub cell_not {
        my( $self, $args ) = @_;

        die unless $args->{cell};

        die if Sudoku::Controller::fact_setting();

        my $cell_index = $self->_find_cell( $args->{cell} );

        return unless $self->{_POSSIBLE}{$args->{value}}{$cell_index};

        $self->{_POSSIBLE}{$args->{value}}{$cell_index} = 0;

        Sudoku::Controller::add_inference( "$self->{_ID}", sub { $self->_check_remaining; } );
    }

    sub register_set {
        my( $self, $set ) = @_;

        push @{$self->{_SETS}}, $set;
    }

    sub exclusive_set {
        my( $self, $args ) = @_;

        foreach my $set( @{$self->{_SETS}} ) {
            next if $set == $args->{set};

            next unless $set->orientation eq $args->{set}->orientation;

            $set->cannot_have( $args->{value} );
        }
    }

    sub get_either_or_pairs {
        my( $self ) = @_;

        my @pairs;

        for my $n( 1 .. 9 ) {
            my @possible = map { $self->{_CELLS}[$_] } grep { $self->{_POSSIBLE}{$n}{$_} } 0 .. 8;

            if( @possible == 2 ) {
                push @pairs, { n => $n, pair => \@possible };
            }
        }

        return @pairs;
    }

    sub _check_remaining {
        my( $self ) = @_;

        my %limited_nbrs;

        # find numbers that are constrained to only a few cells
        for my $n( 1 .. 9 ) {
            my @possible = grep { $self->{_POSSIBLE}{$n}{$_} } 0 .. 8;

            if( not @possible ) {
                die "not possible";
            }

            if( @possible == 1 ) {
                $self->{_CELLS}[$possible[0]]->value( $n, "only cell left in group" );
            }

            if( @possible == 2 ) {
                $limited_nbrs{$n} = join( ',', @possible );
            }

            if( @possible <= 3 ) {
                foreach my $set( @{$self->{_SETS}} ) {
                    my @has = grep { $set->has_cell( $_ ) } map { $self->{_CELLS}[$_] } @possible;

                    if( @has == @possible ) {
                        $set->is_exclusive( { group => $self, value => $n } );
                    }
                }
            }
        }

        if( 1 < keys %limited_nbrs ) {
            my %limited_cells;

            # see if we have pairs of cells that share the same limited numbers
            foreach my $nbr( keys %limited_nbrs ) {
                $limited_cells{ $limited_nbrs{$nbr} }{count}++;
                $limited_cells{ $limited_nbrs{$nbr} }{$nbr} = 1;
            }

            foreach my $cell_pair( keys %limited_cells ) {
                die if $limited_cells{$cell_pair}{count} > 2;

                if( $limited_cells{$cell_pair}{count} > 1 ) {
                    my @nbrs = grep { $_ ne 'count' } keys %{ $limited_cells{$cell_pair} };

                    my $reason = "must be either $nbrs[0] or $nbrs[1]";

                    my( $cell_a, $cell_b ) = split /,/, $cell_pair;

                    for my $n( grep { not exists $limited_cells{$cell_pair}{$_} } 1 .. 9 ) {
                        $self->{_CELLS}[$cell_a]->not_possible( $n, $reason );
                        $self->{_CELLS}[$cell_b]->not_possible( $n, $reason );
                    }
                }
            }
        }

        my %value_pairs;
        my %value_triplets;

        for my $i( 0 .. 8 ) {
            my @possible = $self->{_CELLS}[$i]->get_possible;

            if( @possible == 2 ) {
                push @{$value_pairs{join( ',', @possible )}}, $i;
            }
            elsif( @possible == 3 ) {
                push @{$value_triplets{join( ',', @possible )}}, $i;
            }
        }

        my %chains_of_pairs;

        foreach my $value_pair( keys %value_pairs ) {
            if( @{$value_pairs{$value_pair}} == 2 ) {
                my( $cell_a, $cell_b ) = @{$value_pairs{$value_pair}};
                my( $value_a, $value_b ) = split /,/, $value_pair;

                my $reason = 'cells ' . $self->{_CELLS}[$cell_a]{_ID} . ' and ' . $self->{_CELLS}[$cell_b]{_ID} . ' are exclusive for ' . $value_a . ' and ' . $value_b;

                foreach my $cell( grep { $_ != $cell_a and $_ != $cell_b } 0 .. 8 ) {
                    $self->{_CELLS}[$cell]->not_possible( $value_a, $reason );
                    $self->{_CELLS}[$cell]->not_possible( $value_b, $reason );
                }

                # keep this pair from being picked up when processing
                # value_triplets later
                delete $value_pairs{$value_pair};
            }
            elsif( @{$value_pairs{$value_pair}} == 1 ) {
                my( $cell ) = @{$value_pairs{$value_pair}};
                my( $value_a, $value_b ) = split /,/, $value_pair;

                for( $value_a, $value_b ) {
                    $chains_of_pairs{$_} ||= { cells => {}, links => {} };
                    $chains_of_pairs{$_}{cells}{$cell} = 1;
                }

                $chains_of_pairs{$value_a}{links}{$value_b} = 1;
                $chains_of_pairs{$value_b}{links}{$value_a} = 1;
            }
            elsif( @{ $value_pairs{$value_pair} } ) {
                die "too many cells, not enough values";
            }
        }

        # look for chains of pairs of cells that together create exclusivity for a set of values
        # two cells paired together (detected above) is far more common
CHAIN:
        while( keys %chains_of_pairs > 2 ) {
            my( $start_link ) = keys %chains_of_pairs;
            my %links;
            my @agenda = ( $start_link );

            while( @agenda ) {
                my $link_value = shift @agenda;

                my @others = ( keys %{$chains_of_pairs{$link_value}{links}} );

                if( @others == 1 ) {
                    my( $cell ) = keys %{$chains_of_pairs{$link_value}{cells}};
                    my $other = $others[0];

                    delete $chains_of_pairs{$other}{links}{$link_value};

                    delete $chains_of_pairs{$other}{cells}{$cell};

                    delete $chains_of_pairs{$link_value};

                    next CHAIN;
                }
                elsif( @others == 0 ) {
                    # will happen if there is just a single pair cell
                    delete $chains_of_pairs{$link_value};
                    next CHAIN;
                }

                $links{$link_value} = 1;
                push @agenda, grep { not exists $links{$_} } keys %{$chains_of_pairs{$link_value}{links}};
            }

            my @links = keys %links;

            foreach my $link_value( @links ) {
                foreach my $cell( grep { not exists $chains_of_pairs{$link_value}{cells}{$_} } 0 .. 8 ) {
                    $self->{_CELLS}[$cell]->not_possible( $link_value, "not part of a chain of pairs for this value" );
                }
            }

            delete @chains_of_pairs{@links};
        }

        foreach my $value_triplet( keys %value_triplets ) {
            if( @{$value_triplets{$value_triplet}} == 3 ) {
                my( $cell_a, $cell_b, $cell_c ) = @{$value_triplets{$value_triplet}};
                my( $value_a, $value_b, $value_c ) = split /,/, $value_triplet;

                my $reason = 'cells ' . $self->{_CELLS}[$cell_a]{_ID} . ', ' . $self->{_CELLS}[$cell_b]{_ID} . ', and ' . $self->{_CELLS}[$cell_c]{_ID} . ' are exclusive for ' . $value_a . ', ' . $value_b . ', and ' . $value_c;

                foreach my $cell( grep { $_ != $cell_a and $_ != $cell_b and $_ != $cell_c } 0 .. 8 ) {
                    $self->{_CELLS}[$cell]->not_possible( $value_a, $reason );
                    $self->{_CELLS}[$cell]->not_possible( $value_b, $reason );
                    $self->{_CELLS}[$cell]->not_possible( $value_c, $reason );
                }
            }
            elsif( @{$value_triplets{$value_triplet}} < 3 ) {
                my @cells = @{$value_triplets{$value_triplet}};
                my( $value_a, $value_b, $value_c ) = split /,/, $value_triplet;

                foreach my $pair( "$value_a,$value_b", "$value_a,$value_c", "$value_b,$value_c" ) {
                    if( exists $value_pairs{$pair} ) {
                        push @cells, @{$value_pairs{$pair}};
                    }
                }

                if( @cells > 3 ) {
                    die "too many cells, not enough values";
                }
                elsif( @cells == 3 ) {
                    my( $cell_a, $cell_b, $cell_c ) = @cells;

                    my $reason = 'cells ' . $self->{_CELLS}[$cell_a]{_ID} . ', ' . $self->{_CELLS}[$cell_b]{_ID} . ', and ' . $self->{_CELLS}[$cell_c]{_ID} . ' are exclusive for ' . $value_a . ', ' . $value_b . ', and ' . $value_c;

                    foreach my $cell( grep { $_ != $cell_a and $_ != $cell_b and $_ != $cell_c } 0 .. 8 ) {
                        $self->{_CELLS}[$cell]->not_possible( $value_a, $reason );
                        $self->{_CELLS}[$cell]->not_possible( $value_b, $reason );
                        $self->{_CELLS}[$cell]->not_possible( $value_c, $reason );
                    }
                }
            }
        }
    }
}

{
    # A set is a horizontal or vertical slice of a block
    package Sudoku::Set;

    use strict;
    use warnings;

    sub new {
        my( $proto, $args ) = @_;
        my $class = ref $proto || $proto;

        my $self = {
            _ID => $args->{id},
            _ORIENTATION => $args->{orientation},
            _CELLS => [ $args->{cell1}, $args->{cell2}, $args->{cell3} ],
            _GROUP_A => $args->{a},
            _GROUP_B => $args->{b},
            _EXCLUSIVE => {},
        };

        bless $self, $class;

        $self->{_GROUP_A}->register_set( $self );
        $self->{_GROUP_B}->register_set( $self );

        return $self;
    }

    sub has_cell {
        my( $self, $cell ) = @_;

        ( $self->{_CELLS}[0] == $cell ) ||
        ( $self->{_CELLS}[1] == $cell ) ||
        ( $self->{_CELLS}[2] == $cell )
    }

    sub is_exclusive {
        my( $self, $args ) = @_;

        return if exists $self->{_EXCLUSIVE}{$args->{value}};

        $self->{_EXCLUSIVE}{$args->{value}} = 1;

        if( $args->{group} == $self->{_GROUP_A} ) {
            $self->{_GROUP_B}->exclusive_set( { set => $self, value => $args->{value} } );
        }
        else {
            $self->{_GROUP_A}->exclusive_set( { set => $self, value => $args->{value} } );
        }
    }

    sub orientation {
        my( $self ) = @_;

        return $self->{_ORIENTATION};
    }

    sub cannot_have {
        my( $self, $value ) = @_;

        foreach my $cell( @{$self->{_CELLS}} ) {
            $cell->not_possible( $value, "another set has claimed exclusivity" );
        }
    }
}

for my $i( 0 .. 8 ) {
    push @rows, Sudoku::Group->new( { id => "row $i" } );
    push @columns, Sudoku::Group->new( { id => "column $i" } );
    push @blocks, Sudoku::Group->new( { id => "block $i" } );
}

my @cells;

for my $y( 0 .. 8 ) {
    for my $x( 0 .. 8 ) {
        my $cell = Sudoku::Cell->new( { id => "$x,$y" } );

        push @cells, $cell;

        $rows[$y]->add_cell( { nbr => $x, cell => $cell } );
        $columns[$x]->add_cell( { nbr => $y, cell => $cell } );

        my $block = 3 * int( $y / 3 ) + int( $x / 3 );
        my $nbr = ( $x % 3 ) + 3 * ( $y % 3 );

        $blocks[$block]->add_cell( { nbr => $nbr, cell => $cell } );
    }
}

my @sets;

for my $i( 0 .. 2 ) {
    for my $j( 0 .. 2 ) {
        my $block = $blocks[$i * 3 + $j];

        for my $k( 0 .. 2 ) {
            my( $h1, $h2, $h3, $v1, $v2, $v3 );

            $h1 = $cells[$i * 27 + $k * 9 + $j * 3 + 0];
            $h2 = $cells[$i * 27 + $k * 9 + $j * 3 + 1];
            $h3 = $cells[$i * 27 + $k * 9 + $j * 3 + 2];

            $v1 = $cells[$i * 27 + 0 * 9 + $j * 3 + $k];
            $v2 = $cells[$i * 27 + 1 * 9 + $j * 3 + $k];
            $v3 = $cells[$i * 27 + 2 * 9 + $j * 3 + $k];

            push @sets, Sudoku::Set->new(
                {
                    id => "set h $i $j $k",
                    orientation => 'horizontal',
                    a => $block,
                    b => $rows[$i * 3 + $k],
                    cell1 => $h1,
                    cell2 => $h2,
                    cell3 => $h3,
                }
            );
            push @sets, Sudoku::Set->new(
                {
                    id => "set v $i $j $k",
                    orientation => 'vertical',
                    a => $block,
                    b => $columns[$j * 3 + $k],
                    cell1 => $v1,
                    cell2 => $v2,
                    cell3 => $v3,
                }
            );
        }
    }
}

my $major_v_sep = "+===+===+===+===+===+===+===+===+===+";
my $minor_v_sep = "+---+---+---+---+---+---+---+---+---+";
my $major_h_sep = "H";
my $minor_h_sep = "|";

sub print_table {
    my( $args ) = @_;

    print "\n";

    print "$major_v_sep\n";

    for my $row( 0 .. 8 ) {
        my( $line1, $line2, $line3 ) = ( 'H', 'H', 'H' );

        my $base = $row * 9;

        for my $column( 0 .. 8 ) {
            my $str = $cells[$base + $column]->get_state;

            $line1 .= substr( $str, 0, 3 );
            $line2 .= substr( $str, 3, 3 );
            $line3 .= substr( $str, 6, 3 );

            if( ( $column + 1 ) % 3 ) {
                $line1 .= $minor_h_sep;
                $line2 .= $minor_h_sep;
                $line3 .= $minor_h_sep;
            }
            else {
                $line1 .= $major_h_sep;
                $line2 .= $major_h_sep;
                $line3 .= $major_h_sep;
            }
        }

        print "$line1\n$line2\n$line3\n";

        if( ( $row + 1 ) % 3 ) {
            print "$minor_v_sep\n";
        }
        else {
            print "$major_v_sep\n";
        }
    }

    print "\n";
}

print_table( { cells => \@cells } );

print "command? ";

while( my $command = <> ) {
    if( $command =~ /^([0-8]),([0-8])=([1-9])$/ ) {
        my $nbr = $1 + 9 * $2;

        $cells[$nbr]->value( $3, "given" );

        Sudoku::Controller::inference_loop();
    }
    elsif( $command =~ /^([0-8]),([0-8])!=([1-9])$/ ) {
        my $nbr = $1 + 9 * $2;

        $cells[$nbr]->not_possible( $3, "given" );

        Sudoku::Controller::inference_loop();
    }
    elsif( $command =~ /^e$/ ) {
        for my $i( 0 .. 8 ) {
            my $line = <>;

            $line =~ /^([1-9 ])([1-9 ])([1-9 ])([1-9 ])([1-9 ])([1-9 ])([1-9 ])([1-9 ])([1-9 ])$/ or die;

            if( $1 ne ' ' ) {
                $cells[$i * 9 + 0]->value( $1, "given" );
                print_table( { cells => \@cells } );
            }
            if( $2 ne ' ' ) {
                $cells[$i * 9 + 1]->value( $2, "given" );
                print_table( { cells => \@cells } );
            }
            if( $3 ne ' ' ) {
                $cells[$i * 9 + 2]->value( $3, "given" );
                print_table( { cells => \@cells } );
            }
            if( $4 ne ' ' ) {
                $cells[$i * 9 + 3]->value( $4, "given" );
                print_table( { cells => \@cells } );
            }
            if( $5 ne ' ' ) {
                $cells[$i * 9 + 4]->value( $5, "given" );
                print_table( { cells => \@cells } );
            }
            if( $6 ne ' ' ) {
                $cells[$i * 9 + 5]->value( $6, "given" );
                print_table( { cells => \@cells } );
            }
            if( $7 ne ' ' ) {
                $cells[$i * 9 + 6]->value( $7, "given" );
                print_table( { cells => \@cells } );
            }
            if( $8 ne ' ' ) {
                $cells[$i * 9 + 7]->value( $8, "given" );
                print_table( { cells => \@cells } );
            }
            if( $9 ne ' ' ) {
                $cells[$i * 9 + 8]->value( $9, "given" );
                print_table( { cells => \@cells } );
            }
        }

        Sudoku::Controller::inference_loop();
    }

    if( !$ARGV or $ARGV eq "-" or eof ) {
        print_table( { cells => \@cells } );
        print "command? ";
    }
}

END {
    print_table( { cells => \@cells } );
}

