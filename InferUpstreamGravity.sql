-- Default Parameters
LET $n = 3;
LET $drop = 0.1;

-- GUI
PROMPT TITLE 'Infer Upstream Gravity Inverts';

PROMPT LINE $n 'Number of Steps (upstream): ' DP 0;
PROMPT LINE $drop 'Drop at Manhole (feet): ' DP 1;

PROMPT DISPLAY;

-- Initial all links and nodes as not "selected",
-- except the node currently "selected"
UPDATE [ALL Links] SET $link_selected = 0;
UPDATE [All Nodes] SET $node_selected = 0;
UPDATE SELECTED SET $node_selected = 1;

LET $count = 0;

WHILE $count < $n;
	-- Select the upstream link for every selected node, if and only if the pipe is gravity
	SET us_links.$link_selected = 1 WHERE $node_selected = 1 AND us_links.solution_model='Full';
	
	-- Select the upstream node of the link selected from the previous step
	UPDATE [ALL Links] SET us_node.$node_selected = 1 WHERE $link_selected = 1;
	
	-- select all "selected"
	SELECT FROM [All Links] WHERE $link_selected = 1; 
	SELECT FROM [All Nodes] WHERE $node_selected = 1;
	
	-- Assign DS invert of next pipe upstream
	UPDATE SELECTED SET us_links.ds_invert=ds_links.us_invert+$drop;
	
	-- Define Preferred Slope by Pipe Diameter
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0040 WHERE us_links.conduit_width <= 8;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0028 WHERE us_links.conduit_width = 10;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0022 WHERE us_links.conduit_width = 12;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0015 WHERE us_links.conduit_width = 15;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0014 WHERE us_links.conduit_width = 16;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0012 WHERE us_links.conduit_width = 18;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0010 WHERE us_links.conduit_width = 21;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0008 WHERE us_links.conduit_width = 24;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0006 WHERE us_links.conduit_width = 30;
	UPDATE SELECTED SET us_links.us_invert=us_links.ds_invert+us_links.conduit_length*0.0005 WHERE us_links.conduit_width >= 36;
	
	LET $count = $count + 1;
WEND;

-- select all "selected" (is this needed?)
SELECT FROM [All Links] WHERE $link_selected = 1; 
SELECT FROM [All Nodes] WHERE $node_selected = 1;

-- Set data quality flags
UPDATE SELECTED SET us_links.ds_invert_flag="QIF3";
UPDATE SELECTED SET us_links.us_invert_flag="QIF3";
