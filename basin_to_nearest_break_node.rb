net=WSApplication.current_network
net.transaction_begin 

puts 'Connecting subcatchments to the closest node to centroid:'

subcatchment=net.row_objects('hw_subcatchment').each do |subcatchment| 
x=subcatchment.x
y=subcatchment.y
di=9999999999
dischargeNode=''
if (subcatchment['node_id'] == '')
	node=net.row_objects('hw_node').each do |node| 
		if (node['node_type'].upcase == 'BREAK')
			 tdi=((x-node.x)**2+(y-node.y)**2)**0.5
			  if (tdi<di)
				dischargeNode=node['node_id']
				di=tdi
			  end
		end
	end 
	puts subcatchment['subcatchment_id']+':'+ dischargeNode
	subcatchment['node_id'] = dischargeNode
	subcatchment.write 
	end
end

net.transaction_commit 

puts 'Done.'