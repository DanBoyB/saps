(function($) {
    $(document).ready(function() {
	
	$('#anim_plot').scianimator({
	    'images': ['anim_dir/anim_plot1.png', 'anim_dir/anim_plot2.png', 'anim_dir/anim_plot3.png', 'anim_dir/anim_plot4.png', 'anim_dir/anim_plot5.png', 'anim_dir/anim_plot6.png', 'anim_dir/anim_plot7.png', 'anim_dir/anim_plot8.png', 'anim_dir/anim_plot9.png', 'anim_dir/anim_plot10.png', 'anim_dir/anim_plot11.png', 'anim_dir/anim_plot12.png', 'anim_dir/anim_plot13.png', 'anim_dir/anim_plot14.png', 'anim_dir/anim_plot15.png', 'anim_dir/anim_plot16.png', 'anim_dir/anim_plot17.png', 'anim_dir/anim_plot18.png'],
	    'width': 800,
	    'delay': 1000,
	    'loopMode': 'loop'
	});
	$('#anim_plot').scianimator('play');
    });
})(jQuery);