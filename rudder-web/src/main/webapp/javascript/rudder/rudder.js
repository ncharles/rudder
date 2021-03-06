/*
*************************************************************************************
* Copyright 2011 Normation SAS
*************************************************************************************
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as
* published by the Free Software Foundation, either version 3 of the
* License, or (at your option) any later version.
*
* In accordance with the terms of section 7 (7. Additional Terms.) of
* the GNU Affero GPL v3, the copyright holders add the following
* Additional permissions:
* Notwithstanding to the terms of section 5 (5. Conveying Modified Source
* Versions) and 6 (6. Conveying Non-Source Forms.) of the GNU Affero GPL v3
* licence, when you create a Related Module, this Related Module is
* not considered as a part of the work and may be distributed under the
* license agreement of your choice.
* A "Related Module" means a set of sources files including their
* documentation that, without modification of the Source Code, enables
* supplementary functions or services in addition to those offered by
* the Software.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/agpl.html>.
*
*************************************************************************************
*/

/* Event handler function */

/*
var #table_var#;
        function fnFormatDetails ( id ) {
          var sOut = '<span id="'+id+'" "/>';
          return sOut;
        }

#table_var# = $('').dataTable({
            "asStripeClasses": [ 'color1', 'color2' ],
            "bAutoWidth": ,
            "bFilter" :,
            "bPaginate" :,
            "bLengthChange": ,
            "sPaginationType": ,
            "bJQueryUI": ,
            // "oLanguage": ,
            "aaSorting": [[ , "asc" ]],
            "aoColumns": [             ]
          })

*/

/**
 * Instanciate the tooltip
 * For each element having the "tooltipable" class, when hovering it will look for it's 
 * tooltipid attribute, and display in the tooltip the content of the div with the id 
 * tooltipid 
 */
function createTooltip() {
  $(".tooltipable").tooltip({
			show: {
			  effect: "none",
              delay: 100,
            },
			content: function() {
		      return $("#"+$(this).attr("tooltipid")).html();
		    },
		    position: { 
		      my: "left top+15",
		      at: "right top",
		      collision: "flipfit" 
		    }
  });
}

/* popups */



function callPopupWithTimeout(timeout, popupName, minHeight, minWidth){
	setTimeout("createPopup('"+popupName+"',"+minHeight+","+minWidth+")", timeout);
}

function createPopup(popupName, minHeight, minWidth){
		$("#"+popupName).modal({
			minHeight: minHeight,
			minWidth: minWidth,
			maxHeight: 500,
			maxWidth: 1000
		});
		$('#simplemodal-container').css('height', 'auto');
		correctButtons();
}

/* ignore event propagation (IE compliant) */

function noBubble(event){
    if(event.stopPropagation){
      event.stopPropagation();
    };
    event.cancelBubble=true;
}

/* ignore enter in a field */

function refuseEnter(event)
{
    // IE / Firefox
    if(!event && window.event) {
        event = window.event;
    }
    // IE
    if(event.keyCode == 13) {
        event.returnValue = false;
        event.cancelBubble = true;
    }
    // DOM
    if(event.which == 13) {
        event.preventDefault();
        event.stopPropagation();
    }
}

/* slickboxShowHide */

	$(document).ready(function() {

	var config = {
		selectors: {
			navigation: "#navigation",
			content: "#contenu"
		},

		text: {
			navigation: {
				minimize: "Hide this menu",
				maximize: ""
			}
		},

		image: {
			prefix: contextPath+"/images/",

			navigation: {
				minimize: "puceHide.jpg",
				maximize: "PuceMenuCachee.jpg"
			}
		},

		className: {
			reader: "for-reader",

			minimize: "minimize",
			minimized: "minimized",

			maximize: "maximize",
			maximized: "maximized"
		}
	},

	tmp = null,
	cache = {};


	cache.$navigation = $(config.selectors.navigation);
	cache.$content    = $(config.selectors.content);

	// Generating the navigation's minimize link
	tmp = '<a class="menuHide ' + config.className.minimize + '" title="' + config.text.navigation.minimize + '" href="#"><img src="' + config.image.prefix + config.image.navigation.minimize + '" alt="" /> <span>' + config.text.navigation.minimize + '</span></a>';
	cache.$navigation.prepend(tmp);

	cache.minimize_navigation = {};
	cache.minimize_navigation.$link = $(config.selectors.navigation + " a." + config.className.minimize);
	cache.minimize_navigation.$img  = cache.minimize_navigation.$link.find("img");
	cache.minimize_navigation.$text = cache.minimize_navigation.$link.find("span");

	/**
	 * Showing/Hidding the #navigation
	 */
	cache.minimize_navigation.$link.click(function() {
		var $link = $(this);

		if (cache.$navigation.hasClass(config.className.minimized)) {
			// The navigation is already minimized, maximize!
			cache.$navigation.removeClass(config.className.minimized);
			cache.$content.removeClass(config.className.maximized);

			// Changing the image
			cache.minimize_navigation.$img.prop("src", config.image.prefix + config.image.navigation.minimize);

			// Changing the text value
			cache.minimize_navigation.$text.removeClass(config.className.reader);
			cache.minimize_navigation.$text.text(config.text.navigation.minimize);
			cache.minimize_navigation.$link.prop("title", config.text.navigation.minimize);
		} else {
			// The navigation is maximized, lets minimize it!
			cache.$navigation.addClass(config.className.minimized);
			cache.$content.addClass(config.className.maximized);

			// Changing the image
			cache.minimize_navigation.$img.prop("src", config.image.prefix + config.image.navigation.maximize);

			// Changing the text value
			cache.minimize_navigation.$text.addClass(config.className.reader);
			cache.minimize_navigation.$text.text(config.text.navigation.maximize);
			cache.minimize_navigation.$link.prop("title", config.text.navigation.maximize);
		}

		return false; // Don’t follow the link
	});
});

/* portlet */

$(function() {
	/*
		$(".column").sortable({
			connectWith: '.column'
		});
		$(".column").disableSelection();
	*/

		$(".portlet").addClass("ui-widget ui-widget-content ui-helper-clearfix arrondis")
			.find(".portlet-header")
				.addClass("ui-widget-header arrondishaut")
				.end()
			.find(".portlet-content");

		$(".portlet-header .ui-icon").click(function() {
			$(this).toggleClass("ui-icon-minusthick").toggleClass("ui-icon-plusthick");
			$(this).parents(".portlet:first").find(".portlet-content").toggle();
		});

	});


/**
 * Check all checkbox named name according to the status of the checkbox with id id
 * @param id
 * @param name
 * @return
 */
function jqCheckAll( id, name )
{
   $("input[name=" + name + "][type='checkbox']").prop('checked', $('#' + id).is(':checked'));
}

/* popin */

// increase the default animation speed to exaggerate the effect
	$.fx.speeds._default = 1000;
	$(function() {
		$('#dialog').dialog({
			autoOpen: false,
			position: [250,100],
			width: 535,
			show: '',
			hide: ''		
		});
		$('#openerAccount').click(function() {
			$('#dialog').dialog('open');
			return false;
		});
	});




// Details
	
		$.fx.speeds._default = 1000;
	$(function() {
		$('#dialogDetail').dialog({
			autoOpen: false,
			position: 'center',
			width: 300,
			show: '',
			hide: ''		
		});
		$('.openDetail').click(function() {
			$('#dialogDetail').dialog('open');
			return false;
		});
	});
	


	$(function() {
	    // Bouton rouge
		$('#openAlert').click(function() {
			$('#dialogAlert').modal({
                minHeight:200,
                minWidth: 450
            });
			$('#simplemodal-container').css('height', 'auto');
			return false;
		});

        // Logout
        $('#logout').click(function() {
			$('#dialogLogOut').modal({
                minHeight:100,
                minWidth: 430
            });
			$('#simplemodal-container').css('height', 'auto');
			return false;
		});

	});

/* button */

function correctButtons() {
	$("button, input:submit", "form").button();
	    
	$("button", "").button();
	    
	$("button, input:submit, a", ".whoUser").button();
		
	$("a", ".whoUser").click(function() { return false; });
	
	//$("button, input:submit", ".detailsUser").button();
	//$("button, input:submit", ".popupButton").button();
		
	$("a", ".detailsUser").click(function() { return false; });
	}

function processKey(e , buttonId){
    if (null == e)
        e = window.event ;
    if (e.keyCode == 13)  {
        e.preventDefault();
        document.getElementById(buttonId).click();
        return false;
    }
}

function roundTabs() {
	$(".tabs").tabs();	
	$(".tabsv").tabs();
	$(".tabsv").removeClass('arrondishaut').addClass('ui-tabs-vertical ui-helper-clearfix arrondis');
	$(".tabsv > ul").removeClass('arrondishaut').addClass('arrondisleft');
	$(".tabsv > ul > li").removeClass('arrondishaut').addClass('arrondisleft');
		
	// test auto-ready logic - call corner before DOM is ready
    $('.arrondis').corner("5px");
	$('.arrondishaut').corner("5px top");
	$('.arrondisbas').corner("5px bottom");
	$('.arrondisleft').corner("5px left");
}

/**
 * Move the filter and paginate zones in the location described by tableId_paginate_area and tableId_filter_area
 * @param tableId
 * @return
 */
function moveFilterAndPaginateArea(tableId) {
	$(tableId+"_paginate_area").append($(tableId+"_next")).append($(tableId+"_info")).append($(tableId+"_previous"));

	if ($(tableId+"_filter_area")) {
		$(tableId+"_filter_area").append($(tableId+"_filter"));
	}

}



/**
 * Move the filter and paginate zones in the location described by tableId_paginate_area and tableId_filter_area
 * move the info (1 to 10) to the info area
 * @param tableId
 * @return
 */
function moveFilterAndFullPaginateArea(tableId) {
	$(tableId+"_paginate_area").append($(tableId+"_paginate"));
	$(tableId+"_info_area").append($(tableId+"_info"));
	if ($(tableId+"_filter_area")) {
		$(tableId+"_filter_area").append($(tableId+"_filter"));
	}
	if ($(tableId+"_length")) {
		$(tableId+"_info_area").append($(tableId+"_length"));
  }

}


function dropFilterArea(tableId) {
	$(tableId+"_info").remove();
	$(tableId+"_filter").remove();
	$(tableId+"_length");
}

function activateButtonOnFormChange(containerDivId, buttonId, status) {
	if ("false"==status)
		$('#'+buttonId).prop("disabled", true);
	else 
		$('#'+buttonId).prop("disabled", false);
	
	// all change on the form
	$('#'+containerDivId+' > form').change(function() { $('#'+buttonId).prop("disabled", false);;});
	// This one is for all input (text, textarea, password... and yes, button)
	$('#'+containerDivId+' :input').change(function() { $('#'+buttonId).prop("disabled", false);;});
	// this one is for the checkbox when using IE
	//if ($.browser.msie) 
	//	$('#'+containerDivId+' > form :checkbox').bind('propertychange', function(e) {if (e.type == "change" || (e.type == "propertychange" && window.event.propertyName == "checked")) {  $('#'+buttonId).prop("disabled", false);}});

	// all change on not the form
	$('#'+containerDivId+' :radio').change(function() { $('#'+buttonId).prop("disabled", false);;});
	// This one is for all input (text, textarea, password... and yes, button)
	$('#'+containerDivId+' :input').keyup(function() { $('#'+buttonId).prop("disabled", false);;});

	$('#'+containerDivId+' :checkbox').bind('propertychange', function(e) {if (e.type == "change" || (e.type == "propertychange" && window.event.propertyName == "checked")) {  $('#'+buttonId).prop("disabled", false);}});

}

/**
 *
 */
function activateButtonDeactivateGridOnFormChange(containerDivId, buttonId, gridId, status, optionnalButton) {
	if ("false"==status)
		$('#'+buttonId).prop("disabled", true);
	else
		activateButtonDeactivateGrid(buttonId, gridId);

	// all change on the form
	$('#'+containerDivId+' > form').change(function() { activateButtonDeactivateGrid(buttonId, gridId, optionnalButton);});
	// This one is for all input (text, textarea, password... and yes, button)
	$('#'+containerDivId+' :input').change(function() { activateButtonDeactivateGrid(buttonId, gridId, optionnalButton);});
	// this one is for the checkbox when using IE
	//if ($.browser.msie)
	//	$('#'+containerDivId+' > form :checkbox').bind('propertychange', function(e) {if (e.type == "change" || (e.type == "propertychange" && window.event.propertyName == "checked")) {  activateButtonDeactivateGrid(buttonId, gridId, optionnalButton);}});

	// all change on not the form
	$('#'+containerDivId+' :radio').change(function() { activateButtonDeactivateGrid(buttonId, gridId, optionnalButton);});
	// This one is for all input (text, textarea, password... and yes, button)
	$('#'+containerDivId+' :input').keyup(function() { activateButtonDeactivateGrid(buttonId, gridId, optionnalButton);});

	$('#'+containerDivId+' :checkbox').bind('propertychange', function(e) {if (e.type == "change" || (e.type == "propertychange" && window.event.propertyName == "checked")) {  activateButtonDeactivateGrid(buttonId, gridId, optionnalButton);}});

}

function activateButtonDeactivateGrid(buttonId, gridId, optionnalButton) {
     $('#'+buttonId).prop("disabled", false);
     $('#'+gridId).addClass("desactivatedGrid");

     if ((optionnalButton)&&("" != optionnalButton))
      $('#'+optionnalButton).prop("disabled", true);
}

function scrollToElement(elementId) {
	$('html, body').animate({ scrollTop: $("#"+ elementId).offset().top }, 500);
	
}

/*

Correctly handle PNG transparency in Win IE 5.5 & 6.
http://homepage.ntlworld.com/bobosola. Updated 18-Jan-2006.

Use in <HEAD> with DEFER keyword wrapped in conditional comments:
<!--[if lt IE 7]>
<script defer type="text/javascript" src="rudder.js"></script>
<![endif]-->

*/

var arVersion = navigator.appVersion.split("MSIE")
var version = parseFloat(arVersion[1])
/*
 * Sometimes body is not initiated on IE when javascript is launched
 * default value should be true/false ?
 */
var filters= true ;
if (document.body != null)
	{
	  filters = document.body.filters;
	}

if ((version >= 5.5) && (filters))
{
   for(var i=0; i<document.images.length; i++)
   {
      var img = document.images[i]
      var imgName = img.src.toUpperCase()
      if (imgName.substring(imgName.length-3, imgName.length) == "PNG")
      {
         var imgID = (img.id) ? "id='" + img.id + "' " : ""
         var imgClass = (img.className) ? "class='" + img.className + "' " : ""
         var imgTitle = (img.title) ? "title='" + img.title + "' " : "title='" + img.alt + "' "
         var imgStyle = "display:inline-block;" + img.style.cssText 
         if (img.align == "left") imgStyle = "float:left;" + imgStyle
         if (img.align == "right") imgStyle = "float:right;" + imgStyle
         if (img.parentElement.href) imgStyle = "cursor:hand;" + imgStyle
         var strNewHTML = "<span " + imgID + imgClass + imgTitle
         + " style=\"" + "width:" + img.width + "px; height:" + img.height + "px;" + imgStyle + ";"
         + "filter:progid:DXImageTransform.Microsoft.AlphaImageLoader"
         + "(src=\'" + img.src + "\', sizingMethod='scale');\"></span>" 
         img.outerHTML = strNewHTML
         i = i-1
      }
   }
}

function showParameters(s){
  if(document.getElementById("showParametersInfo" + s).style.display == "none")
    document.getElementById("showParametersInfo" + s).style.display = "block";
  else
    document.getElementById("showParametersInfo" + s).style.display = "none";
}
