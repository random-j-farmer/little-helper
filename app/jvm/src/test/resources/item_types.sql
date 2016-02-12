select it.typeName, ig.groupName, ig.categoryID, ic.categoryName from invTypes it, invGroups ig, invCategories ic where ig.groupID=it.groupID and ig.categoryID=ic.categoryID and ig.categoryID in (2,3,6,18,22,23,25,40,46,65);

