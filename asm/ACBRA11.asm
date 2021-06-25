*          DATA SET ACBRA11    AT LEVEL 202 AS OF 12/09/19                      
*PHASE T62411A                                                                  
*                                                                               
ACBRA11  TITLE '- BRA General download Server'                                  
*                                                                               
* Level change comments                                                         
* ---------------------                                                         
* UK Levels                                                                     
* ---------                                                                     
* TKLU 001 22APR05 Live date for beta release of eBuyer                         
* JFOS 002 10MAY05 ETYPE DOWNLOAD: DEFAULT IND. CHANGED                         
* TKLU 003 12MAY05 add NARRATIVE LIST/DISPLAY download                          
* TKLU 004 19MAY05 <LO01-4363> Improvements to NARRATIVE                        
* YNGX 005 09jun05 <LO01-4239> READ NEW STATUS PURCHASING APPROVER              
* NSHE 006 29JUN05 MAKE CODE FOR XDATA RECORD CONSISTENT                        
* NSHE 007 05JUL05 RETURN EXPENDITURE TYPE LOCKED STATUS                        
* TKLU 008 12JUL05 BUG FIX TO SEARCH ROUTINE                                    
* TKLU 009 18AUG05 eTime preparations                                           
* TKLU 010 06OCT05 eBuyer Defect: UKCR00004290 - search bug fix                 
* NSHE 011 17OCT05 CHANGE TO APPROVER RECORD LAYOUT <LO01-4786>                 
* TKLU 012 18OCT05 EMail address on approver list                               
* TKLU 013 19OCT05 Search bug fix - prevent table overflow                      
* TKLU 014 19OCT05 Search - avoid spaces in account code problem                
* TKLU 015 09NOV05 eBuyer Defect: UKCR00004531 agent data for artists           
* TKLU 016 09NOV05 Genie UKCR00004531 agent data in exp. type list              
* TKLU 017 11NOV05 Genie UKCR00003469 new request for addr's and curr's         
* TKLU 018 17NOV05 Adjustment to level 017                                      
* TKLU 019 19DEC05 <DU01-5092> - article enhancements                           
* TKLU 020 21DEC05 <DU01-4941> - filter names/values d/load (prepared)          
* TKLU 021 21DEC05 <LO01-5021> - Job/List&Search download (prepared)            
* TKLU 022 29DEC05 <DU01-5092> - add artist flag for articles                   
* TKLU 023 29DEC05 <DU01-4941> - some more preparation for filter n&v           
* TKLU 024 04JAN06 <LO01-5021> - some more preparations for job list            
* TKLU 025 04JAN06 Local storage naming convention applied                      
* TKLU 026 10JAN06 <DU01-5021> - Job module additions                           
* TKLU 027 11JAN06 Bug fix to level *26                                         
* TKLU 028 11JAN06 <LO01-5021> - FALink @Tracker dummy                          
* TKLU 029 12JAN06 <LO01-5021> - Job list/search additions                      
* TKLU 030 13JAN06 <LO01-5021> - UserField download and adjustments             
* TKLU 031 16JAN06 <LO01-5021> - Job list/search additions                      
* TKLU 032 18JAN06 <LO01-5021> - FALink @Tracker - mapping added                
* YNGX 033 19JAN06 <UKCR00004914> FALink - workcode limit list added            
* TKLU 033 24JAN06 <LO01-4757> - exp. category return on expend. types          
* TKLU 033 07FEB06 <LO01-4063> - support new Time & Exp. Approver List          
* TKLU 034 13FEB06 <LO01-5021> - new Media Code download                        
* TKLU 035 15FEB06 <LO01-5021> - Filter N/V: empty list / error                 
* TKLU 036 22FEB06 <LO01-5021> - Retrieve office on job user fields             
* NSHE 037 23FEB06 <LO01-4757> - Merging time, claims and jobs                  
* TKLU 038 28FEB06 <UKCR00005412> self approval vs. owner in APPLIS             
* TKLU 039 09MAR06 <LO01-5021> - Job initial call                               
* TKLU 040 16MAR06 <LO01-5021> - Transfer downloads to new Job server           
* TKLU 041 20MAR06 <DU01-4941> - add artist flag on w/c downloads               
* TKLU 042 23MAR06 <UKCR00005795> etype flags settings - bug fix                
* TKLU 043 06APR06 Allow for flexible SJ levels for US (Max=5/5/7)              
* TKLU 044 07APR06 <UKCR00005875> - LimList multiple elements Bug Fix           
* TKLU 045 20APR06 <LO01-5379> PROGRAM NUMBER FROM 22 TO 24                     
* TKLU 046 25APR06 <UKCR00006050> LimList in AccNameSearch                      
* TKLU 047 27APR06 office validation bug fixes on * and $ user IDs              
* NSHE 048 28APR06 return time type rules with account details                  
* YNGX     28APR06 LimList in AccLst, AccNameSearch, WCoLis, MedCdL             
* YNGX     02MAY06 Merge UK and US versions                                     
* TKLU     02MAY06 <UKCR00006408> EType List expense account settings           
* TKLU     08MAY06 VAT rate in account list and search for claims               
* TKLU     10MAY06 Article w/c filter bug fix (bad branch)                      
* TKLU     11MAY06 W/C special request details for estimates                    
* YNGX 049 12MAY06 <UKCR00006640> Filter jobs with media code limlst            
* TKLU 050 15MAY06 <UKCR00006408> - EType additions for list                    
* TKLU 051 16MAY06 make level *50 change optional due to web appl needs         
* TKLU 052 16MAY06 <UKCR00006840> Currency list - minimum and maximum           
* TKLU 053 19MAY06 Expense account status bug fix                               
* TKLU 054 22MAY06 Items - DSECT usage - prefix typo                            
* YNGX 054 22MAY06 <UKCR00006982> - Bug fix in XDATA showing top level          
* TKLU 055 29MAY06 Currency and Office d/load moved to ACMCS20                  
* TKLU 056 31MAY06 A/C names added to Expenditure Type download                 
* TKLU 057 01JUN06 <DU01-4941> - artist filter on items for estimates           
* YNGX 058 01JUN06 <UKCR00006829> - Filter 1R and 1N a/cs with limlst           
* TKLU     12JUN06 Add office code for SJ to a/c details return data            
* TKLU     12JUN06 Pass Pro/Job on articles to get office                       
* TKLU     13JUN06 Item/List for timesheets - adjustments                       
* NSHE     21JUN06 UKCR00007277 Approver shown correctly                        
* NSHE     22JUN06 Bug showing billable allowed for account details             
* NSHE     23JUN06 UKCR00007512 further fixes to support aplimit                
* DKEL 063 19JUL06 UKCR00007832 Enhancements to aplimit (final approval         
*                               + return higher level approvers)                
* TKLU 065 01AUG06 UKCR00008125 For UK call GETALP twice for E and I            
* TKLU     09AUG06 <DU01-5577> Pass/filter draft account status                 
* YNGX 067 08AUG06  Show lowest level 1R account only.                          
* TKLU 068 23AUG06 <LO01-5729> Foreign name support for ETypes and              
*                  AccountDetails                                               
* TKLU 069 23AUG06 UKCR00007708 Pass approver type on expense call              
* TKLU 070 25AUG06 <LO01-5729> Foreign name support on Work Codes               
* TKLU 071 29AUG06 Fixed bug found in <LO01-5639> testing (APPLST)              
* YNGX 072 04SEP06 UKCR00006829 Check View All security in (ACCLST)             
* NSHE 073 04SEP06 UKCR00007061 return FPT and FJT settings                     
* NSHE 074 18SEP06 UKCR00009149 fix bug with approvers returned                 
* TKLU 075 29SEP06 Items artist filters - add for =N + MCS -> BRA               
* NSHE 076 05OCT06 <1050995> FIX SEARCH FOR ACCOUNTS                            
* NSHE 077 10OCT06 <UKCR00009461> PASS BACK APPROVER LIST FOR SEARCHES          
* TKLU 077 09OCT06 <UKCR00009464> - work code search - add code search          
*          10OCT06 US code merged in from Jim Shea                              
* TKLU 078 18OCT06 <UKCR00008537> - add office return to SJ a/c list            
*                  <UKCR00007466> - Input/Output request parameter SG           
* TKLU 079 23OCT06 FINAL RENAME FROM MCS TO BRA                                 
* NSHE 080 23OCT06 FIX PROBLEMS WITH APPROVER LISTS                             
* NSHE 081 30OCT06 FIX ACCOUNT PERSON FOR ALL PROBLEM                           
* TKLU 082 06NOV06 <UKCR00007466> In/Out (see lvl 78) for search, too           
* TKLU 083 17NOV06 <UKCR00009500> Article list call - T/S = eEstimates          
*                  For estimate copy add media code filter to Account           
*                  list and search, also to details                             
* JFOS 084 07DEC06 <DU01-5868> New embedded call for invoice id d/load          
* TKLU 085 03JAN07 Bug fix for ACC/DETAILS call when error                      
*          08JAN07 US Merger                                                    
* TKLU 086 24JAN07 <DU01-4941> BrandOcean estimate user status                  
* TKLU 087 24JAN07 <DU01-4941> AD_DRFT bug fix                                  
* TKLU 088 30JAN07 <LO01-5729> Foreign names d/l for all countries              
* TKLU     01FEB07 <BR10931L> Limit Access bug fix in ACCLST module             
* NSHE 090 06FEB07 <LO01-6033> search using alternative names                   
* TKLU             Work code list type 'Invoices'                               
* TKLU 091 20FEB07 Work code list type 'Expenses' and some bug fixes            
* NSHE 092 28FEB07 <LO01-6033> fix to search for alternative names              
* TKLU 093 15MAR07 <BR11473L> Acc/List: client list via LimList change          
* TKLU 094 16MAR07 <BR11473L> Another change to improve performance             
* TKLU 095 27MAR07 <UKCR00011965> Get high lvl appr if no low lvl exist         
* TKLU 096 29MAR07 <UKCR00011774> Etype supplier d/load currency fix            
* TKLU 097 16APR07 <UKCR00008535> Suppr office from departm list return         
* TKLU 098 27APR07 US merger                                                    
* TKLU 099 08MAY07 <UKCR00012496> ADVALL bug fix                                
* TKLU 100 11JUN07 US merger                                                    
* TKLU 101 14JUN07 <BR12493L> 2D account list bug fix                           
* TKLU 102 20JUN07 <UKCR00012733> Suppress u/l if SG in Acc/Lst&Srch            
*                  and change acc/dtls to receive SG a/c without U/L            
* NSHE 103 25JUN07 <BR12619L> Change person detail call                         
* TKLU 104 20JUL07 <UKCR00013309> W/C foreign name mapping added                
* TKLU 105 24JUL07 <UKCR00012236> Filter value long name field enlarged         
* TKLU 106 06AUG08 <UKCR00013152> Client name + w/c details in Art/Lst          
*                  <UKCR00013591> Media LimList check in Acc/Detail             
*          08AUG07 US merger                                                    
* TKLU 107 17AUG07 <UKCR00013896> 2D d/load fixed for non office set up         
*                  and search fix (Umlaute) for <BR10028Y>                      
* YNGX 108 22JUN07 <LO01-6397> ENABLE OFFICE LEVEL APPROVAL                     
*                  <LO01-6368> ENABLE APPROVERS FOR TIMESHEETS                  
*                              EXPENSE CLAIMS TO BE SET SEPARATELY              
*                  <UKCR00013691> fix approver look up for account list         
* NSHE     16AUG07 <LO01-6698> FORCE NARRATIVE                                  
* TKLU     31AUG07 Add Close status to account details call                     
* NSHE 109 11SEP07 <BR10042Y> Fix duplicate persons                             
* TKLU 110 02OCT07 <UKCR00014403> 'All Order Items' request mode                
* NSHE     04OCT07 <UKCR00014513> Fix account detail call for prod              
* TKLU     08OCT07 <DU01-6906> SJ office in Account/Search                      
*          11OCT07 <UKCR00011748> Account Details: VAT rate on SG               
* TKLU 111 15OCT07 <UKCR00014660> Clear article d/load return block             
*          16OCT07 <BR10055Y> LimList override in Acc/Details - bug fix         
* SMAN 112 16NOV07 <LO01-6813> Support new ALIKOFFC field in ALIRECDs           
* NSHE     16NOV07 <LO01-6577> Fix bug addressing narrative profile             
* TKLU     16NOV07 <UKCR00013306> Acc/List Jobs: BRA estimate filter            
* TKLU 113 29NOV07 <CQTST00021617> Product level office for XDATA               
* TKLU 114 04DEC07 <BR11702D> TSTSET EMPELD filter routine fix                  
* TKLU 115 17DEC07 Get Cli/Pro/Job based GETOPT values for W/C                  
* TKLU 116 21DEC07 <LO01-6973> Commission rate bug fix in W/C call              
* TKLU 117 03JAN08 <BR11779D> Account/List EMPCSTAT bug fix                     
* TKLU 118 30JAN08 <UKCR00015495> Approver/List ALIRECD IO bug fix              
* SMAN     30JAN08 <LO01-6689> Office validation where client and               
*                  products mix offices                                         
* TKLU 119 08FEB08 <LO01-6456> EType Application Filters                        
*          19FEB08 <DU01-7333> Order profiles PO1/PO2 split                     
*          20FEB08 <BR16197L> APPLIST CONAMT call parameter fix                 
*          26FEB08 <BR16254L> bug fix and US Merger (JSHA)                      
* TKLU 121 11MAR08 EType Application Filters fix for limit lists                
* TKLU 122 19MAR08 <BR12168D> ApLimit by office/client IO bug fix               
* TKLU 123 01APR08 <UKCR00015459> XDATA d/l filtering bug fix                   
* TKLU 124 23APR08 <UKCR00015445> WCOLIST for Estimates - use media             
* NSHE 125 22MAY08 <LO01-7638> New opt maint setting to stop job input          
* TKLU 125 12JUN08 <LO01-5729> Foreign name support additions                   
* TKLU 126 04JUL08 <DU01-7767> More foreign name support additions              
*          04AUG08 Update A#ATRA tables from ACPRO71 (programmers use)          
* NSHE 127 08AUG08 <LO01-7743> New job passive structure                        
* MPEN 128 15AUG08 <LO01-7947> ENHANCEMENTS TO SEARCH AND DISPLAY 2D 2P         
* TKLU     03SEP08 <DU01-8040> Article/Items - new UNIT fields                  
* TKLU     11SEP08 Some 2CO adjustments                                         
* NSHE 130 16SEP08 <BR20093L> Bug in account call                               
* JFOS 131 17SEP08 <BR20123L> Bug in account list call                          
* NSHE 132 29SEP08 <UKCR00017715> Bug in account call                           
* NSHE 133 09OCT08 <UKCR00019508> Bug in account name search                    
*                  <UKCR00019513> Use approver in search for expenses           
* TKLU     14OCT08 Internal orders - w/c list support                           
* MPEN     08DEC08 <BR21758L> FIX FOR APPROVER SEARCH                           
* SMAN 135 05FEB09 <BR22981L> Save 'RTN HIGHER LVL APPRVS' setting              
* MPEN 136 25FEB09 <BR23332L> FIX FOR MISSING PPRELD ON ACCOUNT REC             
* NSHE 137 20OCT08 Changes to the JOBPASD structure                             
* TKLU     29OCT08 SE analysis flags to Acc/List, /Search and /Details          
* TKLU     29DEC08 <UKCR00010608> Job opt/maint currency in Acc/Details         
* TKLU     14JAN09 <BR13641D> Foreign 'type of unit' element length fix         
* SMAN     12FEB09 <BR23163L> Bug fix to Approver List                          
* JFOS 138 18FEB09 <LO01-7636> KSV+VAT CODE ON SUPP A/C DETS (GERMANY)          
* MPEN 139 29APR09 <bBR14154D> SAVE OFF CUR CODE AFTER GETACN+PUT COMP          
*                              CODE IN GOBBLOCK WHEN CALLING GETOPT             
* JFOS 140 23APR09 <LO01-7636> FILTER INVOICE APPROVERS IF INVOICES             
* NSHE 140 08MAY09 CHANGES TO APPROVER RECORDS                                  
* MPEN 141 25JUL09 <BR26242L> FIX BUG IN ACCDTL-CLIENT ENTRY NOT ACCEPT         
* MPEN 142 03JUN09 <UKCR00021750> FILTER 2P ACCOUNT LIST RESULTS                
* MPEN     14MAY09 <LO01-8842> CHANGES TO ACCOUNT LIST,SEARCH,DETAIL            
*                              FOR JOB LOCK                                     
* MPEN     21MAY09 <LO01-8928> RETURN OFFICE NAME FOR SJ LEDGER                 
* MPEN     29MAY09 <LO01-8939> WC LIST AND SEARCH MAKE GETOPT CALL              
* MPEN     08JUN09 <LO01-8945> CONTROL JOB LEVEL ENTRY FOR HOUSE EXP            
* MPEN     16JUN09 <LO01-9022> READ 1R PROFILE SETTINGS                         
* MPEN     23JUN09 <LO01-9047> PASS BILLABLE ONLY                               
* MPEN     06JUL09 <LO01-9115> FILTER MASTER JOBS AND RETURN                    
* MPEN     09JUL09 <LO01-9125> FILTER LOCKED AND CLOSED ACCOUNTS                
* TKLU     22JUL09 <LO01-9104> Suppl discount rate in acc list & search         
* TKLU     28JUL09 <LO01-9188> Return 1R level codes & names in Search          
* NSHE 143 29JUL09 <LO01-8973> Add media/supplier/client/order type             
*                              etype/office and dept to order approvers         
* NSHE     31JUL09 <LO01-8974> Limit list by module                             
* MPEN     04AUG09 <LO01-8463> Check whether to send emails                     
* TKLU     03SEP09 Add Job Locks Default values to Acc/Details                  
* TKLU 144 22SEP09 <BR27589L>  ApLimit/Approver look up fix when 'rtrn          
*                              higher lvls' has a 'No' in between               
* TKLU 145 23SEP09 <BR14666D>  W/C data return bug fix (foreign names)          
* MPEN     23SEP09 <UKCR00021192> PASS UNIT IN LANG FOR BOTH ON 2A CALL         
* JFOS 146 25SEP09 <LO01-8948> Invs: Pass RSTSPREA in AD_IRCPJ if set           
* NSHE 148 05OCT09 <UKCR00024760> Ignore extra names when 1R                    
* NSHE 149 12OCT09 <BR28062L> Agency not on offices fix                         
* YNGX 150 14OCT09 <BR28107L> Read GrdList seq. record bug fix                  
* MPEN 151 28OCT09 <LO01-9473> Pass future time allowed setting                 
* JFOS 152 29OCT09 <LO01-9064> New APLIMIT cats: Invs (no PO), Exps             
* NSHE 152 06NOV09 Fix search to get alternative names                          
* NSHE 153 25NOV09 Fix search when looking for multiple words                   
* SMAN 154 21DEC09 <UKCR00015445> Get comm rate for US when adding row          
* NSHE 155 05JAN10 Add new request field to approver look up                    
* SMAN 156 25JAN10 <BR30196L> Small fix to error message in XSEARCH             
* NSHE 157 26JAN10 <BR30405L> Approver look up fix                              
* NSHE 161 28JAN10 Don't return deleted accounts                                
* MPEN     01FEB10 <UKCR00023546> BUG FIXES FOR 2D/2P LOOKUP                    
* NSHE     10FEB10 Enhancement to deal with getopt                              
* NRAK     22MAR10 <BR32000L> alow 10char acc search terms 2 and 3              
* JFOS     14APR10 Invoices RQ_APOVL stays as $ temporarily                     
* SMAN     21APR10 <UKCR00023162> Include CUR code to ACCLST + SEARCH           
* NSHE 162 11MAY10 <UKCR00027312> Fix to income/suspense account                
* SMAN 163 17JUN10 <BR16153D> Bug fix to foreign wc description                 
* MPEN 164 08JUL10 <BR34406L> In perdtl if no location use most recent          
* TKLU 165 04JUN10 <PR000250> Support high lvl suppliers in 'Items'             
* JSHA 165 24JUN10 <UKCR00028329> bug fix                                       
* NSHE     13JUL10 Change to TSJPAS structure                                   
* MPEN 167 30JUL10            read unapp est app and intern est app             
* NSHE     20AUG10 Bug - missing addresses in account downloads and             
* NSHE             1N limit list not applied in account list call               
* NRAK     03SEP10 <UKCR00028967> rounding bug                                  
* NRAK     27SEP10 <UKCR00029446> COUNT HIGH LEVEL/SELF APPROVERS               
* NRAK     28SEP10 <UKCR00029475> missing entries in APRTAB                     
*                             also, rewrite to use equates                      
* JFOS     23NOV10 <BR10034X> return error if u/l only in 20 call               
* NSHE     06JAN11 PR001278 Return telephone and fax numbers                    
* NRAK 168 17JAN11 BR17157D bad length in email extract                         
* SMAN 169 18JAN11 BR39021L Reinstate lines for View All access                 
* NSHE 170 25JAN11 Merge US changes                                             
* JFOS 171 26JAN11 <BR17257D> Fix EX length errors in XSEARCH                   
* NSHE 172 31JAN11 <UKCR21035> Use workcode in acc search/list/detail           
* MPEN 173 09MAR11 <BR17637D> ignore empeld if not person list/search           
* MPEN 174 11MAR11 <BR41075L> preserve AIO7 for XTEAPPL                         
* JFOS     19APR11 <BR17781D> 2CO bug fix in 2D list download                   
* MPEN 175 10JAN11 <PR001318> 1R approval level on page 4 of app record         
*          22JAN11 <PR001420> Amend Getlmt/advall routines                      
* SMAN     04MAR11 <BR17526D> Return locked account (ignore PROSPEC)            
* JFOS     19APR11 <BR17781D> 2CO bug fix in 2D list download                   
* NRAK 176 12MAY11 <BR17976D> EXTEND LVL1/2/3 PARAMETER FIELDS                  
* NSHE 177 25MAY11 <UKCR32151> Don't show duplicate person codes                
* MPEN 178 30AUG11 <BR18502D> Allow 6 char department code                      
* NSHE 179 13JUN11 US fix to support change to limit list                       
* NSHE     15JUN11 US fix to support memo invoices                              
* MPEN     09AUG11 <PR002405> estimate language support                         
* NSHE 180 28OCT11 Send out higher 1R account details when at low level         
* MPEN     23JAN12 <PR002516> Return w/code 3rd party flag                      
* TKLU     06FEB12 <PR001422> New ALIKSCAT 'Mixed' setting (Inv./Exp.)          
* JFOS 181 12APR12 <BR48756L> Expenses approver list now starts at AIO4         
* NSHE 182 15MAY12 Do 181 level change for time and US fixes                    
* NRAK 183 29JUN12 <OT72697L> bad ledger check in acc detail                    
* MPEN 184 12JUN12 <PR002890> Send estimate check setting for list/srch         
* NSHE     31AUG12 <PR003191> Colour coded work code return for account         
* NSHE 185 11JUN13 Fix to order estimate check                                  
* NSHE 186 10JUL13 Fix account search DSSUP-833                                 
* JFOS     18JUL13 <PR003402>New JOBBER:skip 1r-lvl ents on aprv lookup         
* NSHE     26JUL13 DSBO-152 check for deleted master record on search           
* MPEN 187 11OCT13 <DSBO-334> T/S req on pers rec overrides GOTNOJOB            
* NSHE 188 04APR14 US fix                                                       
* NRAK 189 24APR14 <DSBO-790> Fix for item name length                          
* YNGX 191 27MAR14 <DSBO-751> T/S call OFFAL to check person office             
* MPEN     10JUL14 <DSRD-1957> Exclude terminated users from app list           
* JFOS     27JUL14 <DSBO1082> Fix bug validating SJ off/Limlist                 
* NSHE     14OCT14 <DSRD4699> Look at improving workcode list                   
* NRAK     07NOV14 <DSBO-1226> unprintable char in RSTCOSTG                     
* JFOS     03NOV14 <DSBO1215>Acc/Dtl:test X#LLTSUP for Suplrs in ADVALL         
*                             fix unrelated bug in SUPGAP                       
* NSHE 192 24APR15 <DSBO1406> Remove SETFAC                                     
* NSHE 193 08May15 <DSRD-1957> Reinstate removing terminated approvers          
* JFOS 194 08Oct15 <PCA-1989> Support Limit A/C Access. Not SJ,1R etc.          
* NSHE     24Nov15 <DSRD-9533> Add estimate date to item requests               
* TKLU 195 27Jan15 <RD010160>  Performance improvements (GETOPT)                
*                              Unified AGOBLOCB (not AGOBLOCK) use              
*                                                                               
* NSHE 196 31Aug16 DSRD-13089 Don't send finance email address for Aura         
* NSHE 197 01Sep16 DSRD-13089 Remove change at 196 - temp for front end         
* MPEN 198 26Oct16 DSRD-13869 Send approval status on email not                 
* NSHE 199 11Jul18 DSRD-17109 Check whether this module in use                  
* MPEN 200 23Oct18 DSRD-20447 Relink for new DPAPASD                            
* ABID 201 05Dec19 DSRD-23587 Remove code for A#WCOL/A#TEAP/A#APPL as           
*                             code is moved to ACBRA2D                          
* MPEN 202 04Dec19 DSRD-24607 Prevent modifying timeoff lines for mob           
*                                                                               
SVRDEF   CSECT                                                                  
         LKSVR TYPE=D,IDF=Y,REQUEST=*,CODE=CODE,FILES=FILES,           *        
               ABENDLIST=FAILS,                                        *        
               SLOWLIST=SLOWS,WORKERKEY=ACBO,SYSTEM=ACCSYSQ,APPEND=Y,  *        
               SYSPHASE=SYSPHASE,SERVERTYPE=TSTACBO,LOADFACSOFF=Y,     *        
               BLOCKS=(B#WORKD,WORKD,B#SAVED,SAVED)                             
                                                                                
FAILS    DC    C':'                                                             
SLOWS    DC    C':'                                                             
                                                                                
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BO11**,CLEAR=YES,RR=RE                                       
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         LR    R5,R1                                                            
         USING LP_D,R5                                                          
         L     R7,LP_ARUNP         R7=A(RUNPARMS)                               
         USING RUNPARMD,R7                                                      
         XR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
                                                                                
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING SAVED,R8            R8=A(SAVE W/S)                               
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   INIT02                                                           
         L     R9,LP_ABLK1         ONLINE - ROOT PROVIDES WORKD/SAVED           
         ICM   R8,15,RSVRSAVE                                                   
         B     INIT04                                                           
*                                                                               
INIT02   L     R9,RSVRSAVE                                                      
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         SR    R8,R8                                                            
         ICM   R8,3,WORKLEN                                                     
         LA    R8,WORKD(R8)                                                     
INIT04   ST    R8,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
         MVC   ATWA,LP_ATWA                                                     
                                                                                
         MVC   RUNMODE,RUNPMODE    EXTRACT DDLINK/RUNNER CALLING MODE           
         MVC   ACOMFACS,RCOMFACS                                                
         MVC   AMASTC,RMASTC                                                    
         MVC   ARCPRINT,RCPRINT                                                 
         MVC   ABUFFRIN,RBUFFRIN                                                
         MVC   APRINTER,RPRINTER                                                
         DROP  R6,R7                                                            
                                                                                
         MVI   TWAMODE,0                                                        
         MVC   OVERLAY,=C'*WSBO11*'                                             
                                                                                
         ST    R5,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
         EJECT                                                                  
         L     R1,LP_ARUNP         R1=A(RUNPARMS)                               
         USING RUNPARMD,R1                                                      
         CLI   RUNPMODE,RRUNSTRQ   FIRST FOR RUN                                
         BE    RUNSTR                                                           
         CLI   RUNPMODE,RPRCWRKQ   PROCESS WORK                                 
         BE    PRCWRK                                                           
         CLI   RUNPMODE,RRUNREQQ   RUN REQUEST                                  
         BE    RUNREQ                                                           
         J     EXITY                                                            
         DROP  R1                                                               
                                                                                
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    RUNSTR02            NO                                           
         L     RF,ACOMFACS         YES - LOAD FACILITIES OVERLAYS               
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
         L     RF,AMASTC                                                        
         MVC   VACCEMU,MCVACEMU-MASTD(RF)                                       
         J     RUNSTR04                                                         
                                                                                
RUNSTR02 L     RF,AACCFACS                                                      
         MVC   VACCEMU,X_AACCEMU-X_ACCFACSD(RF)                                 
                                                                                
RUNSTR04 GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
         OI    LP_AIND1,LP_AICOM   COMMA OK FOR LIST DELIMITERS                 
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
PRCWRK   XC    QVALUES(QVALUESL),QVALUES                                        
         LA    R0,OVALUES                                                       
         LHI   R1,OVALUESL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R0,DVALUES                                                       
         LHI   R1,DVALUESL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    PRCWRK02                                                         
                                                                                
         LA    R0,WORKD            CLEAR I/O AREAS                              
         AHI   R0,IOAREA1-WORKD                                                 
         LHI   R1,IOAREASL                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
                                                                                
PRCWRK02 XC    TSARABUF,TSARABUF                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* RUN A DOWNLOAD REQUEST                                              *         
***********************************************************************         
                                                                                
RUNREQ   MVI   GIND2,GI2EBUY                                                    
*                                                                               
         GOTOR (#CPYINI,ACPYINI)   (RE)INITIALISE COMPANY VALUES                
*                                                                               
         USING CPXELD,SCPXEL                                                    
         USING CPYELD,SCPYEL                                                    
*                                                                               
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
*&&UK*&& JZ    RUNREQ02                                                         
*&&US*&& JZ    RUNREQ04                                                         
                                                                                
         L     R0,AGOBLOCB                                                      
         AHI   R0,GOADM-GOBLOCK                                                 
         LHI   R1,GOBLOCKX-GOBLOCKD                                             
         SHI   R0,GOADM-GOBLOCK                                                 
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,AMASTC           SET TRACE OPTION                             
         MVC   IOTRACE,MCTRACE-MASTD(RF)                                        
                                                                                
         GOTOR VDATAMGR,DMCB,DMKEY,ACCDIR,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCMST,(4,0),0                               
         GOTOR VDATAMGR,DMCB,DMKEY,ACCARC,(4,0),0                               
                                                                                
*&&UK                                                                           
RUNREQ02 CLI   CUTSYS,X'73'                                                     
         JNE   RUNREQ04                                                         
         MVC   TEMP(3),=X'A60701'                                               
         GOTOR VDATCON,DMCB,(1,TEMP),(1,XL#TODP)                                
         J     RUNREQ06                                                         
*&&                                                                             
RUNREQ04 GOTOR VDATCON,DMCB,(5,0),(1,XL#TODP)                                   
*                                                                               
RUNREQ06 TM    LP_FLAG,LP_FOFFL    Test offline                                 
         JZ    RUNREQX                                                          
         USING X_DPRINT,RF                                                      
         L     RF,ARCPRINT                                                      
         MVC   PDESC(14),=C'ACBRA11 CALLED'                                     
         GOTOR APRINTER                                                         
         DROP  RF                                                               
                                                                                
RUNREQX  XR    RF,RF                                                            
         ICM   RF,3,LP_QMAPN                                                    
         CHI   RF,A#ACCL                 * ACCOUNT LIST *                       
         JE    ACCLST                                                           
         CHI   RF,A#SRCH                 * ACCOUNT NAME SEARCH **               
         JE    SEARCH                                                           
         CHI   RF,A#ADTL                 * ACCOUNT DETAILS **                   
         JE    ACCDTL                                                           
         DC    H'0'                      UNKNOWN REQUEST                        
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LIST (VARIABLE LEDGER)                                      *         
* => uses either search pointers or account records                   *         
***********************************************************************         
                                                                                
ACCLST   DS    0H                  ** LIST OF ACCOUNTS (OFF/USERID) **          
         GOTOR INILBUF             INITIALISE LOCAL BUFFER                      
         MVI   XL#COUNT,0                                                       
         MVI   XL#GLIND,0                                                       
         MVI   XL#ALL,0                                                         
         XC    BYTE1,BYTE1                                                      
         LAY   R0,GENAREA          Clear genarea as used by getopt              
         LHI   R1,GENAREAX-GENAREA                                              
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
ACCL02   GOTOR ACCRUL                                                           
         MVC   XL#APPL,RQ_ALAPL                                                 
         CLI   XL#APPL,RQ_ADEXP    For expenses and time read                   
         JE    *+12                           approver and limit list           
         CLI   XL#APPL,RQ_ADTIM                                                 
         JNE   ACCL04                                                           
         CLC   RQ_ALLDG,=C'1R'     is it 1R?                                    
         JNE   ACCL04              no - don't set approval                      
         MVI   XL#APPR,C'Y'        yes                                          
ACCL04   MVC   XL#LIML,RQ_ALVAS                                                 
         MVI   XL#USAP,NOQ         default user is not an approver              
         MVC   XL#AOFF,SPACES                                                   
*                                                                               
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         XC    ELEMENT,ELEMENT     Clear media code list for job filter         
         LAY   RE,LIACBLK          Clear cli/pro/job list                       
         LHI   RF,L'LIACBLK                                                     
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         MVCL  RE,R0                                                            
         XC    XL#ADR1,XL#ADR1                                                  
*                                                                               
         CLC   RQ_ALLDG,=C'2P'     IF DOING 2P READ 2D LEDGER FIRST             
         JNE   ACCL06                                                           
         MVC   LDGAUL,=C'2D'                                                    
         J     *+10                                                             
*                                                                               
ACCL06   MVC   LDGAUL,RQ_ALLDG                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         JNE   ACCL184             (empty if error)                             
         CLI   LDGAOP,LDGOTRAN                                                  
         JNE   *+8                                                              
         MVI   LDGAOP,LDGONONE                                                  
         CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
         CLC   LDGAUL,=C'2D'                                                    
         JNE   *+16               IF DOING 2D SEARCH SAVE POS AND LEN           
         MVC   SAVEDPOS,LDGAOP    OF OFFICE                                     
         MVC   SAVEDLEN,LDGAL1                                                  
         CLC   RQ_ALLDG,=C'2P'    IF DOING 2D SEARCH DON'T NEED TO READ         
         JNE   ACCL12             ELEMS                                         
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,RQ_ALLDG                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         JNE   ACCL184                                                          
         CLI   LDGAOP,LDGOTRAN                                                  
         JNE   *+8                                                              
         MVI   LDGAOP,LDGONONE                                                  
         CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
*                                                                               
ACCL07A  L     R1,AIO1                                                          
         XR    R0,R0                                                            
         USING LDGRECD,R1                                                       
         LA    R1,LDGRFST                                                       
*                                                                               
         USING LDGELD,R1                                                        
ACCL08   CLI   LDGEL,0             GET DEP. POS/LEN                             
         JNE   *+6                                                              
         DC    H'0'                ERROR NO LEDGER ELEMENT                      
         CLI   LDGEL,LDGELQ                                                     
         JE    ACCL10                                                           
         IC    R0,LDGLN                                                         
         AR    R1,R0                                                            
         J     ACCL08              BUMP TO NEXT ELEM                            
*                                                                               
ACCL10   MVC   SAVELEN2,LDGDLEN    SAVE DEPRATMENT LENGTH                       
         MVC   SAVEPOS2,LDGDPOS    SAVE DEPARTMENT POSITION                     
ACCL12   MVI   XL#LIND,0                                                        
         GOTOR VALAPP,RQ_ALLDG     is this person an approver for this          
         JNE   ACCL14                                        ledger             
         MVI   XL#USAP,YESQ        Yes                                          
                                                                                
ACCL14   CLC   RQ_ALLDG,PRODUL     is it SJ?                                    
         JE    ACCL18                                                           
         CLC   RQ_ALLDG,=C'1R'     is it 1R?                                    
*&&UK*&& JNE   ACCL46                                                           
*&&US                                                                           
         JE    ACCL16                                                           
         CLC   RQ_ALLDG,=C'2P'     is it 2P?                                    
         JNE   ACCL46                                                           
         CLI   XL#APPL,RQ_ADEXP    For expenses show full account               
         JE    ACCL52                                                           
ACCL16   DS    0H                                                               
*&&                                                                             
                                                                                
         CLC   =C'ALL',RQ_ALCLI    Show lowest level 1R account only            
         JNE   ACCL20                                                           
         MVC   RQ_ALCLI,SPACES     Yes - clear office code                      
         MVI   XL#ALL,YESQ                                                      
         J     ACCL46                                                           
*                                                                               
ACCL18   MVC   XL#HOFF,SPACES                                                   
         MVC   XL#COFF,SPACES                                                   
ACCL20   MVC   CSVKEY2,SPACES                                                   
         OC    RQ_ALCLI,SPACES                                                  
         OC    RQ_ALPRO,SPACES                                                  
         OC    RQ_ALSUB,SPACES                                                  
         OI    RQ_ALMED,C' '       (Required???)                                
         MVC   TEMP2,SPACES                                                     
         CLC   RQ_ALCLI,SPACES     Client/Office list request?                  
         JNE   *+12                                                             
         MVI   XL#LIND,1           set level then                               
         J     ACCL22                                                           
         CLC   RQ_ALPRO,SPACES     Product/Dept list request?                   
         JNE   *+12                                                             
*        MVC   TEMP2(L'RQ_ALCLI),RQ_ALCLI                                       
         MVI   XL#LIND,2           set level then                               
         J     ACCL22                                                           
         MVI   XL#LIND,3           set to job/sub-dept level                    
*        MVC   TEMP2(L'RQ_ALCLI),RQ_ALCLI                                       
*&&US                                                                           
         LA    RE,TEMP2            Point to start of TEMP2 for Orders           
         CLI   XL#APPL,RQ_ADORD      Since they send off/dep in prod            
         JNE   *+14                  filed for person lookup for 2P             
         CLC   RQ_ALLDG,=C'2P'                                                  
         JE    *+14                                                             
*                                                                               
         XR    RE,RE                                                            
         IC    RE,LDGAL1                                                        
         LA    RE,TEMP2(RE)                                                     
         LLC   RF,LDGAL2                                                        
         SHI   RF,1                                                             
         BASR  R4,0                                                             
         MVC   0(0,RE),RQ_ALPRO                                                 
         EX    RF,0(R4)                                                         
*&&                                                                             
         CLC   RQ_ALSUB,SPACES     Job/Sub-dept list request                    
         JE    ACCL22                                                           
         MVI   XL#LIND,4           else person list request                     
*                                                                               
ACCL22   MVC   XL#CLVL,XL#LIND     is it SJ?                                    
         CLC   RQ_ALLDG,PRODUL     is it SJ?                                    
         JNE   ACCL46                                                           
                                                                                
ACCL24   OC    CCTPID,CCTPID                                                    
         JZ    ACCL36                                                           
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO7                                        
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO8                                        
         CLI   XL#LIND,3           job list request                             
         JNE   ACCL26                                                           
         GOTOR GETLMT,'LIDTMEDL'   get media limit list for job filter          
ACCL26   GOTOR GETLMT,'LIDTCPJL'   get cli/pro/job limit list                   
         JNE   ACCL30              if client list is empty exit here            
         CLI   XL#LIND,3           job list request                             
         JNE   ACCL28                                                           
         CLI   X#LLTCPJ,X#LLNONQ   if SJ or media is empty exit here            
         JE    ACCL184                                                          
         CLI   X#LLTMED,X#LLNONQ                                                
         JE    ACCL184                                                          
*                                                                               
ACCL28   XC    XL#ADR1,XL#ADR1                                                  
         CLI   XL#LIND,1           client level only                            
         JNE   ACCL36                                                           
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  R4,0                                                             
         MVC   DUB1(0),RQ_ALCLI                                                 
         EX    RE,0(R4)                                                         
         GOTOR SRTCLI              sort client list                             
         JE    ACCL30              empty now?                                   
         LAY   RE,LIACBLK                                                       
         ST    RE,XL#ADR1                                                       
         XC    XL#ACT,XL#ACT                                                    
         J     ACCL32                                                           
*                                                                               
ACCL30   XC    XL#ADR1,XL#ADR1     on empty list                                
         CLI   XL#LIND,3           job list request                             
         JNE   ACCL36                                                           
         CLI   X#LLTCPJ,X#LLNONQ   if SJ or media is empty exit here            
         JE    ACCL184                                                          
         CLI   X#LLTMED,X#LLNONQ                                                
         JE    ACCL184                                                          
         J     ACCL36              carry on to read all                         
*                                                                               
         USING ACTRECD,R2          read clients per list                        
ACCL32   L     RE,XL#ADR1                                                       
         CLC   0(L'ACTKACT,RE),SPACES                                           
         JNH   ACCL184                                                          
         CLC   XL#ACT,0(RE)        same entry as before?                        
         JNE   ACCL34                                                           
         AHI   RE,L'ACTKACT+L'LIDASJOF                                          
         ST    RE,XL#ADR1                                                       
         J     ACCL32                                                           
ACCL34   MVC   XL#ACT,0(RE)                                                     
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         MVC   ACTKACT,0(RE)                                                    
         AHI   RE,L'ACTKACT+L'LIDASJOF                                          
         ST    RE,XL#ADR1                                                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   ACCL32                                                           
         MVC   CSVKEY1,IOKEY                                                    
         J     ACCL82                                                           
*                                                                               
ACCL36   LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         SR    RE,RE                                                            
         IC    RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ACTKACT(0),RQ_ALCLI                                              
         EX    RE,0(RF)                                                         
         MVC   CSVKEY2,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   ACCL184             (empty if error)                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   ACCL184             (empty if error)                             
*                                                                               
         L     R2,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R2)                                           
         USING PPRELD,R1                                                        
         SR    R0,R0                                                            
ACCL38   CLI   PPREL,0                                                          
         JE    ACCL40                                                           
         CLI   PPREL,PPRELQ                                                     
         JE    *+14                                                             
         IC    R0,PPRLN                                                         
         AR    R1,R0                                                            
         J     ACCL38                                                           
         MVC   XL#HOFF,PPRGAOFF                                                 
         OC    XL#HOFF,SPACES                                                   
*                                                                               
ACCL40   CLC   RQ_ALPRO,SPACES                                                  
         JE    ACCL44                                                           
         LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,LDGAL1                                                        
         LA    RF,ACTKACT(RF)                                                   
         LLC   RE,LDGAL2                                                        
         SHI   RE,1                                                             
         BASR  R4,0                                                             
         MVC   0(0,RF),RQ_ALPRO                                                 
         EX    RE,0(R4)                                                         
         MVC   CSVKEY2,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   ACCL184             (empty if error)                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   ACCL184             (empty if error)                             
*                                                                               
         L     R2,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R2)                                           
         USING PPRELD,R1                                                        
         SR    R0,R0                                                            
ACCL42   CLI   PPREL,0                                                          
         JE    ACCL44                                                           
         CLI   PPREL,PPRELQ                                                     
         JE    *+14                                                             
         IC    R0,PPRLN                                                         
         AR    R1,R0                                                            
         J     ACCL42                                                           
         MVC   XL#COFF,PPRGAOFF                                                 
         OC    XL#COFF,SPACES                                                   
*                                                                               
         CLC   XL#COFF,SPACES                                                   
         JE    ACCL44                                                           
         MVC   XL#HOFF,XL#COFF                                                  
         DROP  R1                                                               
*                                                                               
ACCL44   MVC   XL#COFF,SPACES      office setting and security                  
         CLC   XL#HOFF,SPACES                                                   
         JE    ACCL58                                                           
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,XL#HOFF    validate current office                      
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JE    ACCL46                                                           
         DROP  R1                                                               
*                                                                               
         CLI   CUACCS,0            limit access?                                
         JE    ACCL184             no                                           
         CLC   RQ_ALPRO,SPACES                                                  
         JNE   ACCL184                                                          
         LA    R4,ACTRFST-ACTRECD(R2)                                           
         GOTOR CHKOFF                                                           
         JNE   ACCL184                                                          
         MVI   BYTE1,1                                                          
*                                                                               
T        USING ACTRECD,CSVKEY2     Build key for personnel account              
ACCL46   OC    CCTPID,CCTPID                                                    
         JZ    ACCL56                                                           
                                                                                
*&&UK                                                                           
         CLC   RQ_ALLDG,=C'1R'     Is it personnel account?                     
         JE    ACCL50                                                           
         CLC   RQ_ALLDG,=C'2P'     is it 2P?                                    
         JNE   ACCL54                                                           
*&&                                                                             
*&&US                                                                           
         CLC   RQ_ALLDG,=C'2P'     is it 2P?                                    
         JE    ACCL48                                                           
         CLC   RQ_ALLDG,=C'1R'     Is it personnel account?                     
         JNE   ACCL54                                                           
         J     ACCL50                                                           
*&&                                                                             
*                                                                               
ACCL48   MVC   T.ACTKEY,SPACES     FOR 2P BUILD ACCOUNT KEY                     
         MVC   T.ACTKCPY,CUXCPY                                                 
         MVC   T.ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'2P'                            
         CLI   LDGAOP,1            ONLY FOR OFFICE POSITION 1                   
         JNE   ACCL54                                                           
         MVI   XL#LIND,2                                                        
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         CLC   RQ_ALCLI,SPACES     OFFICE IN CLIENT?                            
         JNH   ACCL48A                                                          
         BASR  RF,0                                                             
         MVC   T.ACTKACT(0),RQ_ALCLI                                            
         EX    RE,0(RF)                                                         
         J     ACCL48A                                                          
*                                                                               
ACCL48A  LA    RE,T.ACTKACT                                                     
         XR    RF,RF                                                            
                                                                                
*&&US                                                                           
         CLC   RQ_ALPRO,SPACES     CHECK WHETHER ANY DEPARTMENT CODE            
         JNH   ACCL54                                                           
         MVI   XL#LIND,3                                                        
         LLC   RF,LDGAL2                                                        
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,RE),RQ_ALPRO    MOVE IN OFFICE/DEPARTMENT                    
         EX    RF,0(R1)                                                         
         J     ACCL54                                                           
*&&                                                                             
*&&UK                                                                           
         CLC   RQ_ALPRO,SPACES                                                  
         JNH   ACCL54                                                           
         MVI   XL#LIND,2                                                        
         CLI   SAVEPOS2,X'00'      ANY DEPARTMENT                               
         JE    ACCL54                                                           
         LLC   RF,LDGAOP                                                        
         AR    RE,RF                                                            
         CLI   SAVELEN2,X'00'                                                   
         JE    ACCL54                                                           
         IC    RF,SAVELEN2                                                      
         SHI   RF,1                                                             
         LA    R3,RQ_ALPRO                                                      
         CLI   SAVEDPOS,1          CHECK WHETHER WE HAD OFFICE POSITION         
         JNE   *+10                NO OK TO MOVE IN DEPARTMENT AS IS            
         IC    R0,SAVEDLEN         BUMP ALONG TO STRIP OFF OFFICE               
         AR    R3,R0                                                            
         CLC   RQ_ALPRO,SPACES     CHECK WHETHER ANY DEPARTMENT CODE            
         JNH   ACCL54                                                           
         MVI   XL#LIND,3                                                        
         BASR  R1,0                                                             
         MVC   0(0,RE),0(R3)       MOVE IN DEPARTMENT                           
         EX    RF,0(R1)                                                         
         J     ACCL54                                                           
*&&                                                                             
*                                                                               
ACCL50   GOTOR GETLMT,'LIDT1RAC'   Get personnel limit list                     
         XC    XL#ADR1,XL#ADR1                                                  
ACCL52   CLI   XL#LIND,0                                                        
         JE    ACCL56              Show lowest level 1R account only            
*                                                                               
         MVC   T.ACTKEY,SPACES                                                  
         MVC   T.ACTKCPY,CUXCPY                                                 
         MVC   T.ACTKUNT(2),=C'1R'                                              
         SR    RE,RE                                                            
         IC    RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   T.ACTKACT(0),RQ_ALCLI                                            
         EX    RE,0(RF)                                                         
         CLC   RQ_ALPRO,SPACES                                                  
         JE    ACCL56                                                           
         LLC   RE,LDGAL1                                                        
         LA    RF,T.ACTKACT(RE)                                                 
*&&US                                                                           
         CLI   XL#APPL,RQ_ADORD      Since they send off/dep in prod            
         JNE   *+18                  filed for person lookup for 2P             
         CLC   RQ_ALLDG,=C'2P'                                                  
         JNE   *+8                                                              
         LA    RF,T.ACTKACT        Point to start of ACCNT for Orders           
*&&                                                                             
         LLC   RE,LDGAL2                                                        
         SHI   RE,1                                                             
         BASR  R4,0                                                             
         MVC   0(0,RF),RQ_ALPRO                                                 
         EX    RE,0(R4)                                                         
         CLC   RQ_ALSUB,SPACES                                                  
         JE    ACCL56                                                           
         LLC   RE,LDGAL2                                                        
         LA    RF,T.ACTKACT(RE)                                                 
         LLC   RE,LDGAL3                                                        
         SHI   RE,1                                                             
         BASR  R4,0                                                             
         MVC   0(0,RF),RQ_ALSUB                                                 
         EX    RE,0(R4)                                                         
         J     ACCL56                                                           
*                                                                               
ACCL54   CLC   RQ_ALLDG,=C'1N'     is it non client account ?                   
         JNE   ACCL55                                                           
         GOTOR GETLMT,'LIDTNCLL'   get non client limit list                    
         XC    XL#ADR1,XL#ADR1                                                  
*                                                                               
ACCL55   OC    T.ACTKACT,SPACES    make sure key is spaced                      
         CLC   T.ACTKACT,SPACES    do we have any account filters?              
         JNH   ACCL108             no then don't bother with search             
         DROP  T                                                                
*                                  pointers                                     
ACCL56   CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
         MVC   XL#COFF,RQ_ALCLI    office on non SJ                             
         OC    XL#COFF,SPACES                                                   
*                                                                               
         USING SAPRECD,R2                                                       
ACCL58   LA    R2,IOKEY            try search passives first                    
         XC    SAPKEY,SAPKEY                                                    
         MVI   SAPKTYP,SAPKTYPQ                                                 
         MVC   SAPKCPY,CUXCPY                                                   
         MVC   SAPKUL,LDGAUL                                                    
         MVI   SAPKSUB,SAPKSUBQ                                                 
         MVI   XL#IOMD,0                                                        
         MVI   XL#COUNT,0                                                       
         CLI   XL#LIND,0                                                        
         JE    ACCL60                                                           
         MVC   SAPKLVL,XL#LIND                                                  
         MVC   SAPKACT,CSVKEY2+ACTKACT-ACTRECD                                  
         CLC   RQ_ALLDG,=C'2P'     DEPARTMENT OR STAFF SEARCH?                  
         JE    ACCL64                                                           
         J     ACCL66                                                           
ACCL60   CLI   LDGAOP,1                                                         
         JNE   ACCL62                                                           
         CLC   XL#COFF,SPACES                                                   
         JE    ACCL62                                                           
         MVC   SAPKACT(L'XL#COFF),XL#COFF                                       
         J     ACCL64                                                           
ACCL62   CLI   CUACCS,C'*'         LIMIT ACCESS?                                
         JE    ACCL64                                                           
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+12                                                             
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   SAPKACT(0),1(RF)                                                 
         EX    RE,0(R1)                                                         
         MVC   XL#COFF,SAPKACT                                                  
ACCL64   MVI   SAPKLVL,1                                                        
         CLI   LDGAL2,0                                                         
         JE    ACCL66                                                           
         MVI   SAPKLVL,2                                                        
         CLI   LDGAL3,0                                                         
         JE    ACCL66                                                           
         MVI   SAPKLVL,3                                                        
         CLI   LDGAL4,0                                                         
         JE    ACCL66                                                           
         MVI   SAPKLVL,4                                                        
ACCL66   GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    ACCL70                                                           
         J     ACCLERR1                                                         
ACCL68   OC    XL#ADR1,XL#ADR1                                                  
         JNZ   ACCL32                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   ACCLERR1                                                         
ACCL70   LA    R2,IOKEY                                                         
         CLC   SAPKEY(SAPKACT-SAPKEY),IOKEYSAV                                  
         JNE   ACCL108                                                          
         CLI   XL#LIND,2                                                        
         JL    ACCL72                                                           
         XR    RE,RE                                                            
         IC    RE,LDGAL1                                                        
         CLI   XL#LIND,3                                                        
         JNE   *+8                                                              
         IC    RE,LDGAL2                                                        
         CLI   XL#LIND,4                                                        
         JNE   *+8                                                              
         IC    RE,LDGAL3                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   SAPKACT(0),CSVKEY2+ACTKACT-ACTRECD                               
         EX    RE,0(RF)                                                         
         JE    ACCL74                                                           
         MVI   BYTE1,0                                                          
         J     ACCL108                                                          
ACCL72   CLI   XL#LIND,0                                                        
         JNE   ACCL74                                                           
         CLI   LDGAOP,0                                                         
         JE    ACCL74                                                           
         CLI   LDGAOP,LDGOKEY                                                   
         JH    ACCL74                                                           
         CLI   XL#COFF,C' '                                                     
         JNH   ACCL74                                                           
         XR    RE,RE                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         AHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   SAPKACT(0),XL#COFF                                               
         EX    RE,0(RF)                                                         
         JNE   ACCL108                                                          
ACCL74   MVI   XL#IOMD,1                                                        
         MVC   CSVKEY1,SAPKEY                                                   
         CLI   CUACCS,0            skip if no limit access                      
         JE    ACCL80                                                           
         CLI   XL#LIND,1                                                        
         JH    ACCL80                                                           
         CLI   LDGAOP,0                                                         
         JE    ACCL80                                                           
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
*&&UK                                                                           
         CLI   LDGAOP,LDGOOFLS     office list ledger                           
         JNE   ACCL76                                                           
         MVC   OFFAOFFC,SAPKACT                                                 
         OI    OFFACTRL,OFFOFLIS                                                
         J     ACCL78                                                           
*                                                                               
ACCL76   CLI   LDGAOP,LDGOTRAN                                                  
         JNE   ACCL77                                                           
                                                                                
         CLI   CUACCS,0            Test User limit access                       
         JE    ACCL80                                                           
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    ACCL80                                                           
         TM    CPXSTATA,CPXLACAC   Test limit account access in use             
         JZ    ACCL80                                                           
         MVC   OFFAOPOS,LDGAOP                                                  
         ST    R2,OFFAREC                                                       
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         JNE   ACCL68                                                           
         J     ACCL80                                                           
*&&                                                                             
ACCL77   CLI   LDGAOP,LDGOKEY                                                   
         JH    ACCL80                                                           
         XR    RE,RE                                                            
         IC    RE,LDGAOP                                                        
         LA    RE,SAPKACT-1(RE)                                                 
         LA    RF,0                                                             
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         LA    RF,1                                                             
         BASR  R4,0                                                             
         MVC   OFFAOFFC,0(RE)   MOVE IN OFFICE                                  
         EX    RF,0(R4)                                                         
ACCL78   MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         JNE   ACCL68                                                           
         DROP  R1                                                               
ACCL80   CLI   RQ_ALMED,C' '       apply media filter                           
         JNH   ACCL82                                                           
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         LA    RE,SAPKACT(RE)                                                   
         CLC   RQ_ALMED,0(RE)                                                   
         JNE   ACCL68                                                           
*                                                                               
ACCL82   MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         USING ACTRECD,R2                                                       
         L     R2,AIO1                                                          
         CLC   RQ_ALLDG,PRODUL     SJ ledger?                                   
         JNE   ACCL90                                                           
*                                                                               
         LR    R1,R2                                                            
         CLI   RQ_ALRLK,YESQ                                                    
         JE    ACCL83                                                           
         GOTOR PROSPEC                                                          
         JNE   ACCL68                                                           
ACCL83   GOTOR ESTCHK                                                           
         JNE   ACCL68                                                           
         LA    R1,ACTRFST                                                       
*                                                                               
         USING PPRELD,R1                                                        
         XR    R0,R0                                                            
ACCL84   CLI   PPREL,0                                                          
         JE    ACCL90                                                           
         CLI   PPREL,PPRELQ                                                     
         JE    ACCL86                                                           
         IC    R0,PPRLN                                                         
         AR    R1,R0                                                            
         J     ACCL84                                                           
*                                                                               
ACCL86   MVC   XL#COFF,SPACES                                                   
         MVC   XL#COFF,XL#HOFF                                                  
         CLC   PPRGAOFF,SPACES                                                  
         JH    ACCL88                                                           
         CLI   BYTE1,1                                                          
         JE    ACCL68              NOT INTERESTED                               
         J     ACCL90                                                           
*                                                                               
ACCL88   MVC   XL#COFF,PPRGAOFF                                                 
         USING OFFALD,R1                                                        
         ST    R1,SAVER1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,XL#COFF    validate current office                      
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         L     R1,SAVER1                                                        
         JE    ACCL90                                                           
         DROP  R1                                                               
*                                                                               
         CLI   CUACCS,0            limit access?                                
         JE    ACCL68              no                                           
         XR    RE,RE                                                            
         IC    RE,LDGAL1                                                        
         LA    RF,ACTKACT                                                       
         LA    RF,0(RE,RF)                                                      
         CLI   0(RF),C' '                                                       
         JH    ACCL68                                                           
         LA    R4,ACTRFST                                                       
         GOTOR CHKOFF                                                           
         JNE   ACCL68                                                           
*                                                                               
ACCL90   CLI   RQ_ALCLO,C'O'       only?                                        
         JNE   ACCL92                                                           
         TM    ACTRSTAT,ACTSCLOS   ONLY CLOSED ACCOUNTS                         
         JZ    ACCL68                                                           
         J     ACCL102                                                          
*                                                                               
ACCL92   CLI   RQ_ALCLO,NOQ        No                                           
         JE    *+12                                                             
         CLI   RQ_ALCLO,C' '       BLANK IS NO                                  
         JH    ACCL94                                                           
         TM    ACTRSTAT,ACTSCLOS   DON'T WAN'T CLOSED THEN                      
         JNZ   ACCL68                                                           
*                                                                               
ACCL94   CLI   RQ_ALILA,C'O'       ONLY                                         
         JNE   ACCL96                                                           
         TM    ACTRSTAT,ACTSLOCK   ONLY LOCKED ACCOUNTS                         
         JZ    ACCL68                                                           
         J     ACCL102                                                          
*                                                                               
ACCL96   CLI   RQ_ALILA,NOQ        BLANK IS NO                                  
         JE    ACCL97                                                           
         CLI   RQ_ALILA,C' '       BLANK MEANS NO                               
         JH    ACCL98                                                           
*&&US                                                                           
         CLC   RQ_ALLDG,=C'1N'     is it non client account ?                   
         JE    ACCL96A                                                          
         CLC   RQ_ALLDG,PRODUL     or client level?                             
         JNE   ACCL97                                                           
         CLI   XL#LIND,1                                                        
         JNE   ACCL97                                                           
ACCL96A  TM    SCPYEL+CPYSTATB-CPYELD,CPYXLC1N                                  
         JZ    ACCL98                                                           
*&&                                                                             
ACCL97   TM    ACTRSTAT,ACTSLOCK   IF NO OR BLANK THEN SKIP LOCKED              
         JNZ   ACCL68                                                           
*                                                                               
ACCL98   CLI   RQ_ADYNO,C'O'                                                    
         JNE   ACCL100                                                          
         TM    ACTRSTAT,ACTSDRFT                                                
         JZ    ACCL68                                                           
         J     ACCL102                                                          
*                                                                               
ACCL100  CLI   RQ_ADYNO,C'N'                                                    
         JE    *+12                                                             
         CLI   RQ_ADYNO,C' '                                                    
         JH    ACCL102                                                          
         TM    ACTRSTAT,ACTSDRFT                                                
         JNZ   ACCL68                                                           
                                                                                
ACCL102  TM    ACTRSTAT,ACTSDELT   Don't return deleted accounts                
         JNZ   ACCL68                                                           
         OC    XL#ADR1,XL#ADR1                                                  
         JNZ   ACCL106                                                          
         MVC   WORK(L'ACTKULA),ACTKULA  filter against limit list               
         MVC   WORK+L'ACTKULA(L'XL#COFF),XL#COFF                                
         GOTOR FLTACT,'XL#LIST'    filter against limit list                    
         JE    ACCL104                                                          
         CLI   XL#APPR,YESQ                                                     
         JNE   ACCL68                                                           
         MVC   IOKEY,CSVKEY1       restore key and set for next                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     ACCL68                                                           
ACCL104  CLI   XL#APPR,YESQ                                                     
         JNE   ACCL106                                                          
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
ACCL106  GOTOR ACCSND                                                           
         J     ACCL68                                                           
*                                                                               
ACCL108  CLI   XL#IOMD,1                                                        
         JE    ACCL184                                                          
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY            Read via account records                     
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(2),LDGAUL                                                
         MVI   ACTKACT,X'41'                                                    
         CLI   XL#LIND,1                                                        
         JL    ACCL110                                                          
         JNH   ACCL122                                                          
         MVC   ACTKEY,CSVKEY2      take key from above for lower levels         
         J     ACCL122                                                          
ACCL110  CLC   RQ_ALLDG,=C'2D'                                                  
         JNE   ACCL112                                                          
         CLC   RQ_ALCLI,SPACES     ANY OFFICE PASSED?                           
         JNH   ACCL118                                                          
         XR    RF,RF                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         LHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   ACTKACT(0),RQ_ALCLI SEARCH ON OFFICE                             
         EX    RF,0(RE)                                                         
         J     ACCL122                                                          
*                                                                               
ACCL112  CLC   RQ_ALLDG,=C'2P'                                                  
         JNE   ACCL118                                                          
         CLC   RQ_ALPRO,SPACES     ANY OFFICE DEPARTMENT PASSED?                
         JH    *+14                                                             
         CLC   RQ_ALCLI,SPACES     ANY OFFICE PASSED?                           
         JNH   ACCL118                                                          
         XR    RF,RF               FOR 2D SEARCH ONLY                           
         CLI   LDGAOP,1            ANY OFFICE IN 2P SEARCH                      
         JNE   ACCL116                                                          
         IC    RF,LDGAL1                                                        
         SHI   RF,1                                                             
         CLC   RQ_ALCLI,SPACES     OFFICE IN CLIENT?                            
         JH    ACCL114                                                          
         BASR  RE,0                                                             
         MVC   ACTKACT(0),RQ_ALPRO                                              
         EX    RF,0(RE)                                                         
         J     ACCL116                                                          
*                                                                               
ACCL114  BASR  RE,0                                                             
         MVC   ACTKACT(0),RQ_ALCLI PUT IN OFFICE                                
         EX    RF,0(RE)                                                         
*                                                                               
ACCL116  LA    RE,ACTKACT                                                       
         XR    RF,RF                                                            
         CLI   SAVEPOS2,X'00'      ANY DEPARTMENT                               
         JE    ACCL122                                                          
         IC    RF,SAVEPOS2                                                      
         SHI   RF,1                subtract 1 to get correct position           
         AR    RE,RF                                                            
         CLI   SAVELEN2,X'00'                                                   
         JE    ACCL122                                                          
         IC    RF,SAVELEN2                                                      
         SHI   RF,1                                                             
         LA    R3,RQ_ALPRO                                                      
         CLI   SAVEDPOS,1          CHECK WHETHER WE HAD OFFICE POSITION         
         JNE   *+10                NO OK TO MOVE IN DEPARTMENT AS IS            
         IC    R0,SAVEDLEN         BUMP ALONG TO STRIP OFF OFFICE               
         AR    R3,R0                                                            
         CLC   RQ_ALPRO,SPACES     CHECK WHETHER ANY DEPARTMENT CODE            
         JNH   ACCL122                                                          
         BASR  R1,0                                                             
         MVC   0(0,RE),0(R3)       MOVE IN DEPARTMENT                           
         EX    RF,0(R1)                                                         
         J     ACCL122                                                          
*                                                                               
ACCL118  MVC   XL#COFF,SPACES                                                   
         CLI   CUACCS,C'*'         LIMIT ACCESS?                                
         JNE   ACCL122                                                          
         CLI   LDGAOP,1            OFFPOS=1?                                    
         JNE   ACCL122                                                          
         TM    CPYSTAT4,CPYSOFF2                                                
         JNZ   ACCL120                                                          
         MVC   ACTKACT(1),CUACCS+1                                              
         MVC   XL#COFF,ACTKACT                                                  
         MVI   ACTKACT+L'ACTKACT,X'41'                                          
         J     ACCL122                                                          
*                                                                               
ACCL120  MVC   CSVKEY1,IOKEY       IF 2 CHARACTER OFFICE CHECK LOGON            
         XC    IOKEY,IOKEY         IS OFFICE/OFFICE LIST                        
         USING OFFRECD,IOKEY                                                    
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,CUACCS+1                                                 
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+10                                                             
         MVC   OFFKOFF,CUACCS+2                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         TM    OFFKSTAT,OFFSLIST   IS IT OFFICE LIST?                           
         JNZ   ACCL122                                                          
         MVC   IOKEY,CSVKEY1       IF NOT OFFICE LIST READ OFFICE               
         MVC   ACTKACT(2),CUACCS+1                                              
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+10                                                             
         MVC   ACTKACT(2),CUACCS+2                                              
         MVC   XL#COFF,ACTKACT                                                  
ACCL122  MVC   CSVKEY1(ACTKACT-ACTRECD),ACTKEY                                  
ACCL124  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         JE    ACCL128                                                          
         J     ACCLERR1                                                         
ACCL126  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         JNE   ACCLERR1                                                         
ACCL128  LA    R2,IOKEY                                                         
         CLC   CSVKEY1(ACTKACT-ACTRECD),ACTKEY                                  
         JNE   ACCL184             ACCOUNTS ONLY                                
*&&US                                                                           
         TM    ACTKSTAT,ACTSDELT   DON'T WAN'T DELETED ACCOUNTS                 
         JNZ   ACCL126                                                          
*&&                                                                             
         CLI   RQ_ALCLO,C'O'       only?                                        
         JNE   ACCL128A                                                         
         TM    ACTRSTAT,ACTSCLOS   ONLY CLOSED ACCOUNTS                         
         JZ    ACCL126                                                          
         J     ACCL132                                                          
*                                                                               
ACCL128A CLI   RQ_ALCLO,NOQ        No                                           
         JE    *+12                                                             
         CLI   RQ_ALCLO,C' '       BLANK IS NO                                  
         JH    ACCL128B                                                         
         TM    ACTKSTAT,ACTSCLOS   DON'T WAN'T CLOSED THEN                      
         JNZ   ACCL126                                                          
                                                                                
ACCL128B CLI   RQ_ALILA,C'O'       ONLY                                         
         JNE   ACCL129                                                          
         TM    ACTKSTAT,ACTSLOCK   ONLY LOCKED ACCOUNTS                         
         JZ    ACCL126                                                          
         J     ACCL132                                                          
*                                                                               
ACCL129  CLI   RQ_ALILA,NOQ        BLANK IS NO                                  
         JE    ACCL129B                                                         
         CLI   RQ_ALILA,C' '       BLANK MEANS NO                               
         JH    ACCL129C                                                         
*&&US                                                                           
         CLC   RQ_ALLDG,=C'1N'     is it non client account ?                   
         JE    ACCL129A                                                         
         CLC   RQ_ALLDG,PRODUL     or client level?                             
         JNE   ACCL129B                                                         
         CLI   XL#LIND,1                                                        
         JNE   ACCL129B                                                         
ACCL129A TM    SCPYEL+CPYSTATB-CPYELD,CPYXLC1N                                  
         JZ    ACCL129C                                                         
*&&                                                                             
ACCL129B TM    ACTKSTAT,ACTSLOCK   IF NO OR BLANK THEN SKIP LOCKED              
         JNZ   ACCL126                                                          
                                                                                
ACCL129C CLI   RQ_ADYNO,C'O'                                                    
         JNE   ACCL130                                                          
         TM    ACTKSTAT,ACTSDRFT                                                
         JZ    ACCL126                                                          
         J     ACCL132                                                          
*                                                                               
ACCL130  CLI   RQ_ADYNO,C'N'                                                    
         JNE   ACCL132                                                          
         TM    ACTKSTAT,ACTSDRFT                                                
         JNZ   ACCL126                                                          
                                                                                
ACCL132  CLI   RQ_ALMED,C' '       apply media filter                           
         JNH   ACCL136                                                          
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         LA    RE,ACTKACT(RE)                                                   
         CLC   RQ_ALMED,0(RE)                                                   
         JNE   ACCL126                                                          
*                                                                               
ACCL136  CLC   ACTKEY+CACKWRK-CACRECD(CACKSPAC-CACKWRK),SPACES                  
         JNE   ACCL126                                                          
         CLI   XL#LIND,0           SJ/1R ledger or lowest 1R account?           
         JE    ACCL148             No - OK                                      
         LA    RE,LDGAL4                                                        
         LHI   RF,4                                                             
         CLI   0(RE),L'ACTKACT                                                  
         JE    *+12                                                             
         SHI   RE,1                                                             
         JCT   RF,*-12                                                          
         STC   RF,BYTE3            Save lowest level                            
*                                                                               
         CLI   LDGAL1,L'ACTKACT    Single level account                         
         JNE   *+10                                                             
         XR    RF,RF                                                            
         J     ACCL138                                                          
*                                                                               
         CLC   XL#LIND,BYTE3       Lowest level                                 
         JNE   ACCL140                                                          
         XR    RF,RF                                                            
         IC    RF,BYTE3                                                         
         SHI   RF,2                                                             
ACCL138  LA    RF,LDGAL1(RF)       RE=Penultimate level account                 
         XR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         CLC   ACTKACT(0),CSVKEY2+ACTKACT-ACTRECD                               
         EX    RE,0(R1)                                                         
         JNE   ACCL184                                                          
         TM    ACTKSTAT,ACTSABLP   then ignore higher levels                    
         JNZ   ACCL146                                                          
         J     ACCL126                                                          
*                                                                               
ACCL140  LHI   RF,L'ACTKACT                                                     
         LA    RE,ACTKACT+L'ACTKACT-1                                           
         CLI   0(RE),C' '                                                       
         JE    *+12                                                             
         SHI   RE,1                                                             
         JCT   RF,*-12                                                          
         STC   RF,BYTE2            BYTE2=Length of account                      
*                                                                               
         CLI   XL#LIND,3           sub-dept only ?                              
         JNE   ACCL142                                                          
         CLC   BYTE2,LDGAL3                                                     
         JNE   ACCL126                                                          
         MVC   WORK(L'ACTKACT),SPACES                                           
         XR    R1,R1                                                            
         IC    R1,LDGAL1                                                        
         SHI   R1,1                                                             
         BASR  R4,0                                                             
         MVC   WORK(0),RQ_ALCLI                                                 
         EX    R1,0(R4)                                                         
         IC    R1,LDGAL1                                                        
         LA    RF,WORK(R1)                                                      
         IC    R1,LDGAL2                                                        
         SHI   R1,1                                                             
         BASR  R4,0                                                             
         MVC   0(0,RF),RQ_ALPRO                                                 
         EX    R1,0(R4)                                                         
         IC    R1,LDGAL2                                                        
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         CLC   WORK(0),ACTKACT    check on product/department code              
         EX    R1,0(RF)                                                         
         JE    ACCL146                                                          
         J     ACCL184                                                          
*                                                                               
ACCL142  CLI   XL#LIND,2           products/department only ?                   
         JNE   ACCL144                                                          
         CLC   BYTE2,LDGAL2                                                     
         JNE   ACCL126                                                          
         XR    R1,R1                                                            
         IC    R1,LDGAL1                                                        
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         CLC   RQ_ALCLI(0),ACTKACT check on client/office code                  
         EX    R1,0(RF)                                                         
         JE    ACCL146                                                          
         J     ACCL184                                                          
*                                                                               
ACCL144  CLC   BYTE2,LDGAL1        clients/office only                          
         JNE   ACCL126                                                          
*                                                                               
ACCL146  DS    0H                                                               
*                                                                               
ACCL148  CLC   RQ_ALLDG,=C'2D'                                                  
         JNE   ACCL154                                                          
         CLI   LDGAL1,L'ACTKACT                                                 
         JE    ACCL152                                                          
         MVC   TEMP2(1),LDGAOP                                                  
         NI    TEMP2,FF-LDGOKEY2                                                
         CLI   TEMP2,1                                                          
         JNE   ACCL152                                                          
         LA    RE,ACTKACT+1                                                     
         XR    RF,RF                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+12                                                             
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         CLI   0(RE),C' '          exclude high level accounts                  
         JE    ACCL126                                                          
         CLC   XL#AOFF,SPACES                                                   
         JNH   ACCL152                                                          
         BASR  R1,0                                                             
         CLC   ACTKACT(0),XL#AOFF                                               
         EX    RF,0(R1)                                                         
         JH    ACCL184                                                          
         JE    ACCL152                                                          
         MVC   ACTKACT,SPACES                                                   
         MVC   ACTKACT(2),XL#AOFF                                               
         J     ACCL124                                                          
*                                                                               
ACCL152  TM    ACTKSTAT,ACTSABLP   test low level a/c (2D may be 3-lvl)         
         JZ    ACCL126                                                          
         CLC   RQ_ALCLI,SPACES                                                  
         JNH   ACCL160                                                          
         CLI   LDGAOP,0                                                         
         JE    ACCL160                                                          
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   ACTKACT(0),RQ_ALCLI                                              
         EX    RE,0(RF)                                                         
         JNE   ACCL184                                                          
         MVC   CSVKEY1,ACTKEY                                                   
         J     ACCL170                                                          
*                                                                               
ACCL154  CLC   RQ_ALLDG,=C'2P'                                                  
         JNE   ACCL160                                                          
         CLC   RQ_ALPRO,SPACES     CHECK OFFICE/DEPARTMENT PASSED               
         JH    *+14                                                             
         CLC   RQ_ALCLI,SPACES                                                  
         JNH   ACCL160                                                          
         CLI   LDGAOP,0                                                         
         JE    ACCL160                                                          
         CLI   SAVEPOS2,0                                                       
         JE    ACCL156                                                          
         CLI   SAVELEN2,0                                                       
         JE    ACCL156                                                          
         CLC   RQ_ALPRO,SPACES     CHECK ANY DEPARTMENT CODE PASSED             
         JNH   ACCL156             NO THEN USE OFFICE CODE                      
         XR    RE,RE                                                            
         XR    R0,R0                                                            
         IC    RE,SAVELEN2                                                      
         IC    R0,LDGAL1                                                        
         AR    RE,R0                                                            
         LA    R3,RQ_ALPRO                                                      
         J     ACCL158                                                          
*                                                                               
                                                                                
ACCL156  LLC   RE,LDGAL1                                                        
         LA    R3,RQ_ALCLI                                                      
ACCL158  SHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   ACTKACT(0),0(R3)                                                 
         EX    RE,0(RF)                                                         
         JNE   ACCL184                                                          
         LA    RF,ACTKACT                                                       
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         MVC   CSVKEY1,ACTKEY                                                   
         CLI   0(RF),C' '          IS IT LOW LEVEL ACCOUNT?                     
         JE    ACCL126             THEN DON'T WANT                              
         J     ACCL170                                                          
*                                                                               
ACCL160  MVC   CSVKEY1,ACTKEY                                                   
         TM    ACTKSTAT,ACTSABLP   IGNORE HIGHER LEVELS                         
         JZ    ACCL126                                                          
         CLI   CUACCS,0            skip if no limit access                      
         JE    ACCL170                                                          
         CLI   XL#LIND,0                                                        
         JNE   ACCL170                                                          
         CLI   LDGAOP,0            skip if office not in key                    
         JE    ACCL170                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         CLI   LDGAOP,LDGOOFLS                                                  
         JNE   ACCL162                                                          
         MVC   OFFAOFFC,ACTKACT    Move in office list code                     
         J     ACCL164                                                          
*                                                                               
ACCL162  CLI   LDGAOP,LDGOTRAN                                                  
         JNE   ACCL163                                                          
*&&UK                                                                           
         CLI   CUACCS,0            Test user limit access                       
         JE    ACCL170                                                          
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    ACCL170                                                          
         TM    CPXSTATA,CPXLACAC   Test limit account access in use             
         JZ    ACCL170                                                          
         MVC   OFFAOPOS,LDGAOP                                                  
         ST    R2,OFFAREC                                                       
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         JE    ACCL170                                                          
         J     ACCL165                                                          
*&&                                                                             
ACCL163  CLI   LDGAOP,LDGOKEY                                                   
         JH    ACCL170                                                          
         XR    RE,RE                                                            
         IC    RE,LDGAOP                                                        
         LA    RE,ACTKACT-1(RE)                                                 
         XR    RF,RF                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         LA    RF,1                                                             
         MVC   OFFAOFFC,SPACES     MOVE IN OFFICE                               
         BASR  RE,0                                                             
         MVC   OFFAOFFC(0),0(RE)                                                
         EX    RF,0(RE)                                                         
ACCL164  MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         JE    ACCL170                                                          
ACCL165  MVC   IOKEY,CSVKEY1       restore key and set for next                 
         CLI   LDGAOP,LDGOOFLS                                                  
         JNE   ACCL166                                                          
         MVI   ACTKACT+L'TRNOFFC,FF                                             
         J     ACCL168                                                          
ACCL166  XR    RE,RE                                                            
         IC    RE,LDGAOP                                                        
         LA    RE,ACTKACT(RE)                                                   
         XR    RF,RF                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         AHI   RE,1                                                             
         MVI   0(RE),FF                                                         
ACCL168  GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     ACCL126                                                          
         DROP  R1                                                               
ACCL170  CLI   RQ_ALMED,C' '       apply media filter                           
         JNH   ACCL172                                                          
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         LA    RE,ACTKACT(RE)                                                   
         CLC   RQ_ALMED,0(RE)                                                   
         JNE   ACCL126                                                          
ACCL172  MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         CLI   XL#LIND,0                                                        
         JE    ACCL178                                                          
         LR    R1,R2                                                            
         GOTOR PROSPEC                                                          
         JNE   ACCL126                                                          
         LA    R1,ACTRFST                                                       
         USING PPRELD,R1                                                        
         XR    R0,R0                                                            
ACCL174  CLI   PPREL,0                                                          
         JE    ACCL126                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    ACCL176                                                          
         IC    R0,PPRLN                                                         
         AR    R1,R0                                                            
         J     ACCL174                                                          
                                                                                
ACCL176  MVC   XL#COFF,SPACES                                                   
         MVC   XL#COFF,XL#HOFF                                                  
         CLC   PPRGAOFF,SPACES                                                  
         JNH   ACCL178                                                          
         MVC   XL#COFF,PPRGAOFF                                                 
         DROP  R1                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,XL#COFF    validate current office                      
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JNE   ACCL126                                                          
         DROP  R1                                                               
ACCL178  MVC   WORK(L'ACTKULA),ACTKULA                                          
         MVC   WORK+L'ACTKULA(L'XL#COFF),XL#COFF                                
         GOTOR FLTACT,'XL#LIST'    filter against limit list                    
         JE    ACCL180                                                          
         CLI   XL#APPR,YESQ                                                     
         JNE   ACCL126                                                          
         MVC   IOKEY,CSVKEY1       restore key and set for next                 
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     ACCL126                                                          
ACCL180  CLI   XL#APPR,YESQ                                                     
         JNE   ACCL182                                                          
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
ACCL182  GOTOR ACCSND                                                           
         J     ACCL126                                                          
*                                                                               
ACCL184  DS    0H                  HOUSEKEEPING                                 
         OC    XL#COUNT,XL#COUNT                                                
         JNZ   EXITY                                                            
*        DC    H'0'                                                             
         J     EXITY                                                            
*                                                                               
ACCLERR1 MVC   LP_ERROR,=AL2(AE$ENDOF)           FILE ERROR                     
         J     XERROR                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ACCOUNT NAME SEARCH                                                 *         
***********************************************************************         
                                                                                
SEARCH   DS    0H                  ** ACCOUNT SEARCH **                         
*                                                                               
         GOTOR XSEARCH                                                          
         JE    EXITY                                                            
         MVI   LP_RMODE,LP_RERRR                                                
         MVI   LP_EMSYS,6                                                       
*                                                                               
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
         EJECT                                                                  
***********************************************************************         
* Check for office code in LIDTPOFC                                   *         
***********************************************************************         
CHKOFF   DS    0H                                                               
         USING LIDELD,R4                                                        
CHKOFF02 CLI   LIDEL,0                                                          
         JE    CHKOFFN             No valid office code found                   
         CLI   LIDEL,LIDELQ                                                     
         JNE   CHKOFF04                                                         
         CLI   LIDTYPE,LIDTPOFC      Product office codes                       
         JE    CHKOFF06                                                         
CHKOFF04 XR    RF,RF                                                            
         IC    RF,LIDLN                                                         
         AR    R4,RF                                                            
         J     CHKOFF02                                                         
CHKOFF06 XR    R0,R0                                                            
         LA    RF,LIDDATA-LIDELD                                                
         XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         SR    R1,RF               R1=Length of LIDDATA                         
         XR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         DR    R0,RF               R1=No. of office codes in LIDDATA            
         LR    R3,R1                                                            
         LA    R4,LIDDATA          R4 points to the office codes                
*                                                                               
         USING OFFALD,R1                                                        
CHKOFF08 L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,0(R4)                                                   
         MVI   OFFAACT,OFFAVAL                                                  
         LR    R0,RE                                                            
         GOTO1 VOFFAL                                                           
         LR    RE,R0                                                            
         JE    CHKOFFY                                                          
         LA    R4,L'XL#HOFF(R4)                                                 
         JCT   R3,CHKOFF08                                                      
         J     CHKOFFN                                                          
*                                                                               
CHKOFFY  CR    RB,RB                                                            
         J     *+6                                                              
CHKOFFN  LTR   RB,RB                                                            
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ACCOUNT DETAILS DOWNLOAD                                            *         
***********************************************************************         
* uses AIO1 for all IOs, non updative                                 *         
***********************************************************************         
                                                                                
ACCDTL   DS    0H                  ** ACCOUNT DETAILS DOWNLOAD **               
*                                                                               
         GOTOR INILBUF             INITIALISE LOCAL BUFFER                      
         GOTOR ACCRUL                                                           
         LA    R0,AD_VALS                                                       
         XR    R1,R1                                                            
         XR    RE,RE                                                            
         LHI   R1,AD_VALQ-1                                                     
         LA    RF,C' '                                                          
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         MVI   AD_ANAD,NOQ                                                      
         MVI   AD_ANAP,NOQ                                                      
         MVI   AD_DLOCB,NOQ                                                     
         XC    AD_DFTND,AD_DFTND                                                
         XC    AD_DFTEX,AD_DFTEX                                                
         MVC   XL#APPL,RQ_ADAPL                                                 
         OC    RQ_ADACC,SPACES                                                  
         CLI   RQ_ADACC+2,C' '     TEST ANY ACCOUNT CODE SET                    
         JH    ACCDT00                                                          
         MVC   LP_ERROR,=AL2(AE$MIACC) 'MISSING ACCOUNT'                        
         J     XERROR                                                           
ACCDT00  CLI   RQ_ADMED,C' '                                                    
         JNH   ACCDT01                                                          
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         LA    RE,RQ_ADACC+2(RE)                                                
         CLI   0(RE),C' '                                                       
         JNH   ACCDT02                                                          
         CLC   RQ_ADMED,0(RE)                                                   
         JE    ACCDT02                                                          
         MVC   LP_ERROR,=AL2(AE$ECSCP)                                          
         J     XERROR                                                           
*                                                                               
ACCDT01  CLI   RQ_ADLED,C'V'       VAT=SG                                       
         JNE   ACCDT02                                                          
         MVC   TEMP2(12),RQ_ADACC                                               
         MVC   RQ_ADACC(2),SGQ                                                  
         MVC   RQ_ADACC+2(12),TEMP2                                             
*                                                                               
ACCDT02  MVC   TEMP2(14),RQ_ADACC                                               
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,TEMP2                                                     
         CLI   RQ_ADLED,C'X'                                                    
         JNE   ACCDT03                                                          
         MVC   LDGAUL,=C'1R'                                                    
         J     ACCDT04                                                          
ACCDT03  CLC   RQ_ADACC(L'LDGAUL),=C'2P' STAFF SEARCH?                          
         JNE   ACCDT04                                                          
         MVC   LDGAUL,=C'2D'       DO 2D LEDGER FIRST                           
ACCDT04  GOTOR (#SETLDG,ASETLDG)                                                
         JE    ACCDT06                                                          
ACCDT05  MVC   LP_ERROR,=AL2(AE$INLDG)                                          
         J     XERROR                                                           
*                                                                               
ACCDT06  CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
         MVC   SAVEDPOS,LDGAOP     SAVE POSITION/LENGTH OF OFF ON 2D            
         MVC   SAVEDLEN,LDGAL1                                                  
         CLC   RQ_ADACC(L'LDGAUL),=C'2P' IF DOING 2D DON'T NEED TO READ         
         JNE   ACCDT07            ELEMS                                         
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,RQ_ADACC                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         JNE   ACCDT05                                                          
         CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
*                                                                               
         L     R1,AIO1                                                          
         XR    R0,R0                                                            
         USING LDGRECD,R1                                                       
         LA    R1,LDGRFST                                                       
*                                                                               
         USING LDGELD,R1                                                        
ACCDT06A CLI   LDGEL,0             GET DEP. POS/LEN                             
         JNE   *+6                                                              
         DC    H'0'                ERROR NO LEDGER ELEMENT                      
         CLI   LDGEL,LDGELQ                                                     
         JE    ACCDT06B                                                         
         IC    R0,LDGLN                                                         
         AR    R1,R0                                                            
         J     ACCDT06A            BUMP TO NEXT ELEM                            
*                                                                               
ACCDT06B MVC   SAVELEN2,LDGDLEN    SAVE DEPARTMENT LENGTH                       
         MVC   SAVEPOS2,LDGDPOS    SAVE DEPARTMENT POSITION                     
*                                                                               
ACCDT07  CLI   RQ_ADLED,C'X'                                                    
         JNE   ACCDT12                                                          
         GOTOR PERDTL                                                           
         JE    ACCDT08                                                          
         MVC   LP_ERROR,FULL2                                                   
         J     XERROR                                                           
                                                                                
ACCDT08  MVC   XL#ULA(L'XL#UNT+L'XL#LDG),=C'1R'                                 
         L     R1,ALOCEL                                                        
         MVI   0(R1),X'FF'         Mark element so don't read again             
                                                                                
         L     R0,AIO4             copy person record to IO4 from IO1           
         LA    R1,IOLENQ                                                        
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
ACCDT10  L     R1,ALOCEL           retrieve office                              
         MVI   0(R1),X'FF'         Mark element so don't read again             
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                adjust for EX                                
         BASR  RF,0                                                             
         MVC   XL#ACT(0),LOCOFF-LOCELD(R1)                                      
         EX    RE,0(RF)                                                         
         MVC   AD_1ROFF,LOCOFF-LOCELD(R1)                                       
         AHI   RE,1                UNADJUST                                     
         LA    RF,XL#ACT                                                        
         AR    RF,RE                                                            
         LLC   R2,ONERL2L                                                       
         SR    R2,RE                                                            
         SHI   R2,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,RF),LOCDEPT-LOCELD(R1)                                       
         EX    R2,0(RE)                                                         
         MVC   AD_1RDPT,LOCDEPT-LOCELD(R1)                                      
         AHI   R2,1                                                             
         AR    RF,R2                                                            
         LLC   RE,ONERL2L                                                       
         LLC   R2,ONERL3L                                                       
         SR    R2,RE                                                            
         SHI   R2,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,RF),LOCSUB-LOCELD(R1)                                        
         EX    R2,0(RE)                                                         
         MVC   AD_1RSUB,LOCSUB-LOCELD(R1)                                       
         AHI   R2,1                                                             
         AR    RF,R2                                                            
         LLC   R2,ONERL3L                                                       
         LA    R1,L'ACTKACT                                                     
         SR    R1,R2                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   0(0,RF),TEMP2                                                    
         EX    R1,0(RE)                                                         
         MVC   TEMP2,XL#ULA                                                     
         CLI   RQ_ADAPL,RQ_ADEXP   Only want cost profiles for expenses         
         JNE   ACCDT14                                  module                  
         GOTOR (#CSTPRF,ACSTPRF),DMCB,XL#ACT                                    
         L     RE,ACOBLOCK                                                      
         USING COBLOCKD,RE                                                      
         MVC   AD_XAPP,COCAE                                                    
         MVC   AD_XMIL,COMIL                                                    
         DROP  RE                                                               
         J     ACCDT14                                                          
*                                                                               
ACCDT12  GOTOR SSJOFF              USES AIO1                                    
         GOTOR GETPOF              USES AIO1                                    
         MVC   AD_OFFC,XL#COFF                                                  
         MVC   TEMP,SPACES                                                      
         MVC   TEMP,TEMP2          SAVE OFF TEMP2 BEFORE CALLING GETACN         
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'AD_OFFC),AD_OFFC                                         
         GOTOR (#GETOFN,AGETOFN)   USES AIO3                                    
         MVC   AD_OFFN,TEMP2       GET OFFICE NAME                              
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2,TEMP          RESTORE TEMP2                                
         MVC   XL#ULA,TEMP2                                                     
*                                                                               
ACCDT14  GOTOR (#GETACN,AGETACN)   USES AIO3                                    
         CLC   AGYCURR,TEMP2+36                                                 
         JE    ACCDT16                                                          
         MVC   AD_CURC,TEMP2+36                                                 
         OC    AD_CURC,SPACES                                                   
         CLI   TEMP2+36,ASTCANY                                                 
         JNE   ACCDT16                                                          
         MVC   AD_CURC,=CL3'***'                                                
ACCDT16  MVC   AD_NAME,TEMP2                                                    
         MVC   AD_FNAM,SPACES                                                   
         CLC   TEMP2(36),SPACES                                                 
         JH    ACCDT18                                                          
         MVC   LP_ERROR,=AL2(AE$ACTNF)   FILE ERROR                             
         J     XERROR                                                           
                                                                                
ACCDT18  XC    XL#AOFF,XL#AOFF                                                  
                                                                                
         L     R0,AIO2             COPY RECORD READ BY GETACN TO AIO2           
         LA    R1,IOLENQ           SO WE CAN USE GETACN AGAIN...                
         L     RE,AIO3                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING ACTRECD,R4                                                       
         L     R4,AIO2                                                          
         CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
         CLI   LDGAOP,LDGONONE                                                  
         JE    ACCDT32                                                          
         CLI   LDGAOP,LDGOKEY                                                   
         JH    ACCDT24                                                          
                                                                                
         XR    RE,RE                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         AHI   RE,1                                                             
                                                                                
         CLC   RQ_ADACC(L'LDGAUL),=C'2D'                                        
         JE    ACCDT20                                                          
         CLC   RQ_ADACC(L'LDGAUL),=C'2P'                                        
         JNE   ACCDT22                                                          
         CLI   SAVEDPOS,1          CHECK 2D OFFICE POSITION IS 1                
         JNE   ACCDT22                                                          
*                                                                               
ACCDT20  CLI   LDGAOP,1            ONLY FOR OFFICE POSITION 1                   
         JNE   ACCDT22                                                          
         LLC   RF,LDGAOP                                                        
         LA    RF,ACTKACT-1(RF)                                                 
         BASR  R1,0                                                             
         MVC   XL#AOFF(0),0(RF)                                                 
         EX    RE,0(R1)                                                         
         CLC   RQ_ADIOF,SPACES     CHECK OFFICE IS VALID                        
         JNH   ACCDT28                                                          
         CLC   XL#AOFF,SPACES                                                   
         JNH   ACCDT28                                                          
         BASR  R1,0                                                             
         CLC   RQ_ADIOF(0),XL#AOFF    DEPARTMENT MUST HAVE SAME OFFICE          
         EX    RE,0(R1)                                                         
         JE    ACCDT22                                                          
         MVC   LP_ERROR,=AL2(AE$DIDOF) DON'T INPUT DIFFERENT OFFICES            
         J     XERROR                                                           
*                                                                               
ACCDT22  CLC   RQ_ADACC(L'LDGAUL),=C'2P' ONLY FOR 2P REQUEST                    
         JNE   ACCDT24                                                          
         CLI   SAVEPOS2,X'00'      ANY DEPARTMENT                               
         JE    ACCDT24                                                          
         LLC   RE,LDGAL2                                                        
         SHI   RE,1                                                             
         LLC   RF,LDGAOP           Check dept matches                           
         LA    RF,ACTKACT-1(RF)                                                 
         BASR  R1,0                                                             
         MVC   XL#TDPT(0),0(RF)                                                 
         EX    RE,0(R1)                                                         
         OC    XL#TDPT,SPACES                                                   
         OC    RQ_ADEPT,SPACES                                                  
         CLC   XL#TDPT,SPACES                                                   
         JNH   ACCDT28                                                          
         CLC   RQ_ADEPT,SPACES                                                  
         JNH   ACCDT28                                                          
         CLC   RQ_ADEPT,XL#TDPT                                                 
         JE    ACCDT28                                                          
         MVC   LP_ERROR,=AL2(AE$INVDP) INVALID DEPARTMENT CODE                  
         J     XERROR                                                           
*                                                                               
ACCDT24  CLI   LDGAOP,LDGOPROF     Is it client office pos                      
         JNE   ACCDT26             No                                           
         MVC   XL#AOFF,XL#COFF     Yes - use office extracted                   
         J     ACCDT28                                                          
*                                                                               
ACCDT26  CLI   LDGAOP,LDGOTRAN                                                  
         JNE   ACCDT27                                                          
*&&UK                                                                           
         CLI   CUACCS,0            Test user limit access                       
         JE    ACCDT32                                                          
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    ACCDT32                                                          
         TM    CPXSTATA,CPXLACAC   Test limit account access in use             
         JZ    ACCDT32                                                          
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOPOS,LDGAOP                                                  
         ST    R4,OFFAREC                                                       
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         JE    ACCDT32                                                          
         J     ACCDT29                                                          
*&&                                                                             
ACCDT27  CLI   LDGAOP,LDGOOFLS     Is it office list account set up             
         JNE   ACCDT32             No                                           
         MVC   XL#AOFF,ACTKACT     Yes - office list code always                
*                                             at position 1                     
ACCDT28  CLI   CUACCS,0            Limit access?                                
         JE    ACCDT32             No - no need to check code                   
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,XL#AOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
*&&UK                                                                           
         CLI   LDGAOP,LDGOOFLS     If ledger office list                        
         JNE   *+8                                                              
         OI    OFFACTRL,OFFOFLIS   Want to validate office list code            
*&&                                                                             
         GOTO1 VOFFAL              Validate office                              
         JE    ACCDT32                                                          
ACCDT29  CLC   RQ_ALLDG,PRODUL     SJ ledger?                                   
         JNE   ACCDT30                                                          
         DROP  R1                                                               
*                                                                               
         LA    R4,ACTRFST                                                       
         GOTOR CHKOFF                                                           
         JE    ACCDT32                                                          
ACCDT30  MVC   LP_ERROR,=AL2(AE$SECLK)   Security lock out                      
         J     XERROR                                                           
                                                                                
ACCDT32  CLI   RQ_ADVLL,NOQ              don't check                            
         JNE   ACCDT40                                                          
         GOTOR ADVALL                                                           
         JE    ACCDT40                                                          
         CLI   RQ_ADLED,C'X'       1R account                                   
         JNE   ACCDT38                                                          
         MVC   WORK(L'XL#ULA),XL#ULA                                            
         MVC   WORK+L'XL#ULA(L'XL#AOFF),XL#AOFF                                 
         GOTOR ADVAPP                                                           
         JE    ACCDT42                                                          
         L     R1,AIO4                                                          
         AHI   R1,PERRFST-PERRECD  LOCATE ELEMENTS                              
         USING LOCELD,R1                                                        
                                                                                
ACCDT34  CLI   LOCEL,0                                                          
         JE    ACCDT38                                                          
         CLI   LOCEL,LOCELQ                                                     
         JE    ACCDT36                                                          
         LLC   R0,LOCLN                                                         
         AR    R1,R0                                                            
         J     ACCDT34                                                          
                                                                                
ACCDT36  ST    R1,ALOCEL          STORE MOST RECENT LOCATION                    
         MVC   TEMP2(14),RQ_ADACC                                               
         J     ACCDT10                                                          
*                                                                               
ACCDT38  MVC   LP_ERROR,=AL2(AE$ACLIM)   not on user limit list                 
         J     XERROR                                                           
                                                                                
ACCDT40  MVI   XL#JLIND,NOQ                                                     
         CLI   RQ_ADLED,C'P'       FIX -ADACC CAN BE PERSON CODE 'SJ*'          
         JNE   ACCDT42                                                          
         CLC   RQ_ADACC(2),PRODUL  check for SJ product                         
         JNE   ACCDT42                                                          
         CLI   RQ_ADAPL,RQ_ADJOB   Jobs module only?                            
         JNE   ACCDT42                                                          
         LLC   R1,PCLILEN                                                       
         LA    R1,RQ_ADACC+2(R1)                                                
         CLC   0(3,R1),SPACES                                                   
         JE    ACCDT42                                                          
         LLC   R1,PPROLEN                                                       
         LA    R1,RQ_ADACC+2(R1)                                                
         CLC   0(3,R1),SPACES                                                   
         JH    ACCDT42                                                          
         MVI   AD_JLE,NOQ                                                       
         MVI   AD_JLO,NOQ                                                       
         MVI   AD_DLOCB,NOQ                                                     
         MVI   AD_JLT,NOQ                                                       
         MVI   AD_JLA,NOQ                                                       
         MVI   AD_JLX,NOQ                                                       
         MVI   XL#JLIND,YESQ                                                    
                                                                                
ACCDT42  CLI   RQ_ADRLK,YESQ       Return locked account?                       
         JE    ACCDT44                                                          
         L     R1,AIO2                                                          
         GOTOR PROSPEC                                                          
         JE    ACCDT44                                                          
         MVC   LP_ERROR,=AL2(AE$INACP)                                          
         J     XERROR                                                           
ACCDT44  L     R4,AIO2                                                          
*                                                                               
         MVI   AD_DRFT,NOQ                                                      
         MVI   AD_CLOS,NOQ                                                      
         MVI   AD_LOC,NOQ                                                       
         TM    ACTRSTAT,ACTSDRFT   DRAFT                                        
         JZ    ACCDT46                                                          
         MVI   AD_DRFT,YESQ                                                     
*&&US*&& CLI   XL#APPL,RQ_ADTIM    Is it the TIME Application?                  
*&&US*&& JE    JBDFERR1            Display error                                
*&&US*&& CLI   XL#APPL,RQ_ADEXP    Is it the Expense Application?               
*&&US*&& JE    JBDFERR1            Display error                                
ACCDT46  DS    0H                                                               
         TM    ACTRSTAT,ACTSCLOS   CLOSED                                       
         JZ    *+8                                                              
         MVI   AD_CLOS,YESQ                                                     
         TM    ACTRSTAT,ACTSLOCK   LOCKED                                       
         JZ    *+8                                                              
         MVI   AD_LOC,YESQ                                                      
                                                                                
         LA    R2,ACTRFST                                                       
         USING ADRELD,R2                                                        
         XC    AADREL,AADREL                                                    
         XC    AOATEL,AOATEL                                                    
         MVI   BYTE4,NOQ                                                        
ACCDT48  CLI   ADREL,0             TEST EOR                                     
         JE    ACCDT106                                                         
         CLI   ADREL,ADRELQ                                                     
         JE    ACCDT52                                                          
         CLI   ADREL,OATELQ                                                     
         JE    ACCDT54                                                          
         CLI   ADREL,JOBELQ                                                     
         JE    ACCDT56                                                          
         CLI   ADREL,SPAELQ                                                     
         JE    ACCDT58                                                          
         CLI   ADREL,RSTELQ                                                     
         JE    ACCDT70                                                          
         CLI   ADREL,FFTELQ                                                     
         JE    ACCDT88                                                          
*&&UK                                                                           
         CLI   ADREL,XNMELQ                                                     
         JE    ACCDT60                                                          
         CLI   ADREL,RATEVATQ                                                   
         JE    ACCDT62                                                          
         CLI   ADREL,ASTELQ                                                     
         JE    ACCDT64                                                          
         CLI   ADREL,RATEDSCQ                                                   
         JE    ACCDT102                                                         
         CLI   ADREL,DEXELQ                                                     
         JE    ACCDT104                                                         
*&&                                                                             
ACCDT50  LLC   R0,ADRLN                                                         
         AR    R2,R0                                                            
         J     ACCDT48                                                          
                                                                                
ACCDT52  ST    R2,AADREL           STORE ADDRESS OF ADDRESS ELEMENT             
         J     ACCDT50                                                          
                                                                                
         USING OATELD,R2                                                        
ACCDT54  CLI   RQ_ADAPL,RQ_ADORD   TEST ORDERS                                  
         JNE   ACCDT50                                                          
         CLI   OATSUB,OATSUB5Q     ORDER DELIVERY ADDRESS                       
         JNE   ACCDT50                                                          
         ST    R2,AOATEL                                                        
         J     ACCDT50                                                          
                                                                                
         USING JOBELD,R2                                                        
ACCDT56  MVI   AD_BOE,NOQ                                                       
         CLI   JOBLN,JOBLN3Q                                                    
         JL    ACCDT50                                                          
         MVI   AD_MJO,NOQ                                                       
         TM    JOBSTA2,JOBSMST      IS THIS A MASTER JOB?                       
         JZ    *+8                                                              
         MVI   AD_MJO,YESQ          THEN PASS TO WEB APP TO DECIDE              
*&&UK                                                                           
         TM    JOBSTA1,JOBSMCSE                                                 
         JZ    ACCDT50                                                          
         MVI   AD_BOE,YESQ                                                      
         J     ACCDT50                                                          
*&&                                                                             
*&&US                                                                           
         TM    JOBSTA1,JOBSMCSE                                                 
         JZ    *+8                                                              
         MVI   AD_BOE,YESQ                                                      
                                                                                
         TM    JOBSTA2,JOBSREJ                                                  
         JO    JBRJERR1                                                         
         J     ACCDT50                                                          
*&&                                                                             
         USING SPAELD,R2                                                        
ACCDT58  CLI   SPATYPE,SPATMJOB                                                 
         JNE   ACCDT50                                                          
         MVC   AD_MJC,SPAAACT                                                   
         J     ACCDT50                                                          
                                                                                
*&&UK                                                                           
         USING XNMELD,R2                                                        
ACCDT60  DS    0H                                                               
         CLI   RQ_ADLED,C'X'       1R account                                   
         JE    ACCDT50                                                          
         XR    R1,R1                                                            
         ICM   R1,1,XNMSUBL                                                     
         JZ    ACCDT50                                                          
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   AD_FNAM(0),XNMSUBN  Pass foreign name                            
         EX    R1,0(RE)                                                         
         J     ACCDT50                                                          
                                                                                
         USING RATELD,R2                                                        
ACCDT62  CURED (B2,RATRATE),(L'AD_VATR,AD_VATR),0,ZERO=YES,ALIGN=LEFT           
         J     ACCDT50                                                          
                                                                                
         USING ASTELD,R2                                                        
ACCDT64  DS    0H                                                               
         MVI   AD_13BVI,NOQ                                                     
         MVI   AD_ISFO,NOQ                                                      
         TM    ASTSTAT1,ASTISFOR                                                
         JZ    *+8                                                              
         MVI   AD_ISFO,YESQ                                                     
         CLI   RQ_ADLED,C'S'       TEST SUPPLIER CALL                           
         JNE   ACCDT50                                                          
         CLI   RQ_ADAPL,RQ_ADINV   TEST INVOICES                                
         JNE   ACCDT50                                                          
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         JNE   ACCDT50                                                          
*&&UK                                                                           
         CLI   AST13VAT,C' '       TEST VAT CODE SET                            
         JNH   *+14                                                             
         MVC   AD_VATC(1),AST13VAT                                              
*&&                                                                             
         MVI   AD_13BVI,YESQ                                                    
         CLI   ASTKSVTY,0          TEST KSV TYPE SET                            
         JNH   ACCDT50                                                          
         MVC   FULL1,XL#TODP                                                    
         CLC   RQ_ADIDT,SPACES     TEST INVOICE DATE PASSED                     
         JNH   ACCDT66                                                          
         GOTOR VDATCON,DMCB,(0,RQ_ADIDT+2),(1,FULL1)                            
ACCDT66  LA    RF,WORK             GET RATE FOR KSV TYPE                        
         USING CONBLKD,RF                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONFLD,CONFKSV      KSV CALL                                     
         MVI   CONACTN,CONAGETQ    GET KSV DETAILS                              
         MVI   CONILEN,L'ASTKSVTY  LENGTH OF CODE                               
         LA    RE,ASTKSVTY                                                      
         STCM  RE,15,CONIADD       INPUT ADDRESS                                
         LA    RE,TEMP                                                          
         STCM  RE,15,CONOADD       OUTPUT ADDRESS                               
         MVC   CONCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   CONKSVDT,FULL1      EFFECTIVE KSV DATE                           
         GOTO1 VCONVERT,CONBLK                                                  
         JE    *+14                                                             
ACCDT68  MVC   LP_ERROR,=AL2(AE$NOKSV)                                          
         J     XERROR                                                           
         DROP  RF                                                               
         ZAP   DUB1,TEMP(3)                                                     
         CURED (P8,DUB1),(L'AD_KSVR,AD_KSVR),0,ZERO=YES,ALIGN=LEFT              
                                                                                
         J     ACCDT50                                                          
*&&                                                                             
         USING RSTELD,R2                                                        
ACCDT70  MVC   AD_CSTG,RSTCOSTG                                                 
         CLI   AD_CSTG,X'80'       unexplained unprintable chars in             
         JNE   *+8                    many cli/pro records                      
         MVI   AD_CSTG,C' '                                                     
*&&US                                                                           
         CLC   AD_CSTG,SPACES      Any Costing group?                           
         JH    *+16                                                             
         TM    RSTSTAT2,RSTSYCST   cost postings for expenses ledger            
         JZ    *+8                                                              
         MVI   AD_CSTG,YESQ                                                     
*&&                                                                             
         TM    RSTSTAT1,RSTSEADD                                                
         JZ    *+8                                                              
         MVI   AD_ANAD,YESQ                                                     
         TM    RSTSTAT1,RSTSGPEI                                                
         JZ    *+8                                                              
         MVI   AD_ANAP,YESQ                                                     
*&&UK                                                                           
         TM    RSTSTAT2,RSTSMILE                                                
         JZ    *+8                                                              
         MVI   AD_ANAM,YESQ                                                     
*&&                                                                             
         CLI   RQ_ADLED,C'P'                                                    
         JNE   ACCDT71                                                          
         CLC   RQ_ADACC(2),PRODUL                                               
         JE    ACCDT74                                                          
ACCDT71  MVI   AD_FPT,NOQ                                                       
         MVI   AD_SGAP,NOQ                                                      
         MVI   AD_AQAP,NOQ                                                      
         MVI   AD_FJT,NOQ                                                       
         TM    RSTSTAT4,RSTSJREA   TEST JOB REQ'D FOR EXP ANALYSIS              
         JZ    *+8                                                              
         MVI   AD_FJT,YESQ                                                      
                                                                                
         CLI   RSTLN,RSTLN3Q       CHECK RIGHT LENGTH ELEMENT                   
         JL    ACCDT72                                                          
         TM    RSTSTAT5,RSTSPREA   TEST PRODUCT REQUIRED                        
         JZ    *+8                                                              
         MVI   AD_FPT,YESQ                                                      
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   AD_SGAP,YESQ                                                     
         TM    RSTSTAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   AD_AQAP,YESQ                                                     
*&&                                                                             
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPQN   NO GAP SUPPLIER CONTROLS                     
         JZ    *+8                                                              
         MVI   AD_SGAP,NOQ                                                      
         TM    RSTSTAT7,RSTGAPAN   NO ACKNOWLEDGE/QUERY                         
         JZ    *+8                                                              
         MVI   AD_AQAP,NOQ                                                      
         TM    RSTSTAT7,RSTGAPQY   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   AD_SGAP,YESQ                                                     
         TM    RSTSTAT7,RSTGAPAY   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   AD_AQAP,YESQ                                                     
*&&                                                                             
*                                                                               
ACCDT72 GOTOR SUPGAP,DMCB,ACTKEY,AD_SGAP,AD_AQAP                                
                                                                                
ACCDT74  CLI   RQ_ADAPL,RQ_ADTIM   TIMESHEETS?                                  
         JNE   ACCDT78                                                          
         MVI   AD_FUTA,NOQ                                                      
         CLI   RSTLN,RSTLN3Q       CHECK RIGHT LENGTH ELEMENT                   
         JL    ACCDT76                                                          
         TM    RSTSTAT7,RSTSFUTM   FUTURE TIME ALLOWED                          
         JZ    ACCDT76                                                          
         MVI   AD_FUTA,YESQ                                                     
         J     ACCDT78                                                          
ACCDT76  CLI   RQ_ADFTM,YESQ       DO WE ONLY WANT FUTURE TIME ACS              
         JE    NOFUTURE            YES - ERROR AS NOT ONE                       
ACCDT78  CLI   RSTLN,RSTLN3Q       CHECK RIGHT LENGTH ELEMENT                   
         JL    ACCDT50                                                          
         CLI   RQ_ADLED,C'P'                                                    
         JNE   ACCDT50                                                          
         CLC   RQ_ADACC(2),PRODUL                                               
         JNE   ACCDT50                                                          
*                                  CHECK WHETHER JOB LOCKED FROM ORDERS         
ACCDT80  CLI   RQ_ADAPL,RQ_ADORD   ARE WE IN ORDERS?                            
         JNE   ACCDT82                                                          
         TM    RSTLSTAT,RSTLSORQ                                                
         JZ    ACCDT50             NOT LOCKED                                   
         J     ORDERR              YES THEN SKIP                                
*                                  CHECK WHETEHR JOB LOCKED FROM EST            
ACCDT82  CLI   RQ_ADAPL,RQ_ADEST   ARE WE IN ESTIMATES?                         
         JE    *+12                                                             
         CLI   RQ_ADAPL,RQ_ADEJL   ESTIMATES JOB BILLING                        
         JNE   ACCDT84                                                          
         TM    RSTLSTAT,RSTLSESQ                                                
         JZ    ACCDT50             NOT LOCKED                                   
         J     ESTERR              YES THEN SKIP                                
*                                                                               
ACCDT84  CLI   RQ_ADAPL,RQ_ADTIM   ARE WE IN TIMESHEETS?                        
         JNE   ACCDT86                                                          
         TM    RSTLSTAT,RSTLSBIQ   LOCKED FROM BILLING?                         
         JZ    *+12                                                             
         MVI   AD_DLOCB,YESQ       SET NOT ALLOWED MATERIALS IN TIME            
         J     ACCDT50                                                          
         TM    RSTLSTAT,RSTLSTIQ                                                
         JZ    ACCDT50             NOT LOCKED                                   
         J     TIMERR              YES THEN SKIP                                
*                                                                               
ACCDT86  CLI   RQ_ADAPL,RQ_ADEXP   ARE WE IN EXPENSES?                          
         JNE   ACCDT50                                                          
         TM    RSTLSTAT,RSTLSEXQ   LOCKED FROM EXTERNAL POSTINGS?               
         JZ    ACCDT50             NOT LOCKED                                   
         J     EXPERR              YES THEN SKIP                                
*                                                                               
         USING FFTELD,R2                                                        
ACCDT88  DS    0H                                                               
*&&UK                                                                           
         CLI   FFTTYPE,FFTTVATC    VAT CODE                                     
         JE    *+12                                                             
         CLI   FFTTYPE,FFTTG13B    13B VAT CODE                                 
         JNE   ACCDT90                                                          
         CLI   RQ_ADAPL,RQ_ADINV   TEST INVOICES                                
         JNE   ACCDT50                                                          
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         JNE   ACCDT50                                                          
         LA    RF,AD_VATC                                                       
         CLI   AD_VATC,C' '        MAY BE A 13B CODE FROM ASTEL ALREADY         
         JNH   *+8                                                              
         AHI   RF,1                SO APPEND OTHERS                             
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,RF),FFTDATA     UP TO FOUR MORE                              
         EX    RE,0(R1)                                                         
         J     ACCDT50                                                          
*&&                                                                             
*                                                                               
ACCDT90  DS    0H                                                               
*&&US                                                                           
ACCDT92  CLI   FFTTYPE,FFTTBEML    USE ORDER AUTHORISER IF NOT                  
         JNE   ACCDT94             FOUND USE EMAIL ON 1ST PAGE                  
         MVI   BYTE4,YESQ                                                       
         J     ACCDT96                                                          
*&&                                                                             
ACCDT94  CLI   BYTE4,YESQ          HAVE WE FOUND ORDER AUTHORISER               
         JE    ACCDT98             YES                                          
*&&UK*&& CLI   FFTTYPE,FFTTPEML    EMAIL?                                       
*&&US*&& CLI   FFTTYPE,FFTTEML                                                  
         JNE   ACCDT98                                                          
ACCDT96  MVC   AD_EMADD,SPACES                                                  
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   AD_EMADD(0),FFTDATA                                              
         EX    RF,0(R1)                                                         
         J     ACCDT50                                                          
                                                                                
ACCDT98 CLI    FFTTYPE,FFTTPFAX    FAX?                                         
         JNE   ACCDT100                                                         
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   AD_FAX(0),FFTDATA                                                
         EX    RF,0(R1)                                                         
         J     ACCDT50                                                          
                                                                                
ACCDT100 CLI   FFTTYPE,FFTTPTEL    TELEPHONE?                                   
         JNE   ACCDT50                                                          
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   AD_TELE(0),FFTDATA                                               
         EX    RF,0(R1)                                                         
         J     ACCDT50                                                          
         DROP  R2                                                               
*                                                                               
         USING RATELD,R2                                                        
ACCDT102 CURED (B2,RATRATE),(L'AD_DISC,AD_DISC),0,ALIGN=LEFT                    
         J     ACCDT50                                                          
*                                  DUE DATE EL                                  
         USING DEXELD,R2                                                        
ACCDT104 CLI   RQ_ADAPL,RQ_ADINV   TEST INVOICES                                
         JNE   ACCDT50                                                          
         CLI   RQ_ADLED,C'S'       SUPPLIER ACCOUNT                             
         JNE   ACCDT124                                                         
         GOTOR GETDDF,DMCB,DEXELD,AD_DDXP    RETURN DUE DATE FORMULA            
         J     ACCDT50                                                          
                                                                                
ACCDT106 L     R2,AADREL                                                        
         OC    AOATEL,AOATEL                                                    
         JZ    ACCDT112                                                         
         L     R2,AOATEL                                                        
         USING OATELD,R2                                                        
*&&UK                                                                           
         XR    R1,R1                                                            
         ICM   R1,1,OATNUM         N'ADDRESS LINES                              
         JZ    ACCDT116                                                         
         CHI   R1,4                                                             
         JNH   *+8                                                              
         LA    R1,4                                                             
         LA    RF,OATADD1                                                       
         LA    RE,AD_ADD1                                                       
         J     ACCDT114                                                         
*&&                                                                             
*&&US                                                                           
         TM    OATSTAT,OATCSZ                                                   
         JZ    ACCDT108                                                         
         MVC   AD_ADD3,SPACES                                                   
         MVC   AD_ADD3(L'OATCITY),OATCITY                                       
         CLC   AD_ADD3,SPACES                                                   
         JNH   ACCDT108                                                         
         LA    R1,AD_ADD3+L'AD_ADD3                                             
         ST    R1,FULL1                                                         
         AHI   R1,-1               Back up 1 byte to get in the field           
         CLI   0(R1),C' '          Find last significant character              
         JH    *+12                                                             
         AHI   R1,-1                                                            
         J     *-12                                                             
         L     RE,FULL1                                                         
         CR    RE,R1                                                            
         JNH   ACCDT108                                                         
         MVI   1(R1),C','          put in comma                                 
         AHI   R1,3                Increase length to incl comma/space          
         CR    RE,R1                                                            
         JNH   ACCDT108                                                         
         SR    RE,R1               Do we have enough room for ST                
         CHI   RE,L'OATSTATE                                                    
         JL    ACCDT108                                                         
         MVC   0(L'OATSTATE,R1),OATSTATE                                        
         AHI   R1,L'OATSTATE+1                                                  
         L     RE,FULL1                                                         
         SR    RE,R1               Do we have enough room for ZIP               
         CHI   RE,L'OATZIP                                                      
         JL    ACCDT108                                                         
         MVC   0(L'OATZIP,R1),OATZIP                                            
         AHI   R1,L'OATZIP                                                      
         L     RE,FULL1                                                         
         SR    RE,R1               Do we have enough room for EXT               
         CHI   RE,L'OATZIPRN                                                    
         JL    ACCDT108                                                         
         MVC   0(L'OATZIPRN,R1),OATZIPRN                                        
                                                                                
ACCDT108 LA    R1,2                                                             
         LA    RF,OATLINE1                                                      
         LA    RE,AD_ADD1                                                       
ACCDT110 MVC   0(L'AD_ADD1,RE),0(RF)                                            
         AHI   RE,L'AD_ADD1                                                     
         AHI   RF,L'OATLINE1                                                    
         JCT   R1,ACCDT110                                                      
         J     ACCDT116                                                         
*&&                                                                             
         USING ADRELD,R2                                                        
ACCDT112 XR    R1,R1                                                            
         ICM   R1,1,ADRNUM         N'ADDRESS LINES                              
         JZ    ACCDT116                                                         
         CHI   R1,4                                                             
         JNH   *+8                                                              
         LA    R1,4                                                             
         LA    RF,ADRADD1                                                       
         LA    RE,AD_ADD1                                                       
                                                                                
ACCDT114 MVC   0(L'ADRADD1,RE),0(RF)                                            
         AHI   RE,L'OATLINE1                                                    
         AHI   RF,L'ADRADD1                                                     
         JCT   R1,ACCDT114                                                      
                                                                                
ACCDT116 CLI   RQ_ADAPL,RQ_ADINV   TEST INVOICES                                
         JNE   ACCDT124                                                         
         CLI   RQ_ADLED,C'S'       SUPPLIER ACCOUNT                             
         JNE   ACCDT124                                                         
         CLC   AD_DDXP,SPACES      TEST DUE DATE FORMULA FOUND                  
         JH    ACCDT124                                                         
         MVC   FULL1,LDGAL1        SET LEVLENS                                  
ACCDT118 LA    R4,IOKEY            SEARCH LOW TO HIGH A/CS                      
         MVC   ACTKEY,SPACES                                                    
         LA    R1,FULL1+L'FULL1-1                                               
         LA    R0,L'FULL1                                                       
         BASR  RF,0                                                             
         CLI   0(R1),0                                                          
         JH    ACCDT120                                                         
         AHI   R1,-1                                                            
         BCTR  R0,RF                                                            
         J     ACCDT124            NOT FOUND                                    
ACCDT120 MVC   ACTKCPY,CUXCPY                                                   
         XR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         MVI   0(R1),0             CLEAR FOR NEXT TIME                          
         AHI   RF,1                = -1 FOR EX, +2 FOR U/L                      
         BASR  R1,0                                                             
         MVC   ACTKUNT(0),RQ_ADACC                                              
         EX    RF,0(R1)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   ACCDT124                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JNE   ACCDT124                                                         
         L     R4,AIO3                                                          
         LA    R2,ACTRFST                                                       
         USING DEXELD,R2                                                        
         XR    R0,R0                                                            
ACCDT122 CLI   DEXEL,0                                                          
         JE    ACCDT118                                                         
         CLI   DEXEL,DEXELQ                                                     
         JE    *+14                                                             
         IC    R0,DEXLN                                                         
         AR    R2,R0                                                            
         J     ACCDT122                                                         
                                                                                
         GOTOR GETDDF,DMCB,DEXELD,AD_DDXP   GET DUE DATE FORMULA                
ACCDT124 GOTOR GETAGN,DMCB,RQ_ADACC,AD_AGAC,AD_AGAD1                            
*                                                                               
ACCDT126 CLI   RQ_ADLED,C'P'       PRODUCTION ACCOUNT?                          
         JNE   ACCDT150                                                         
         CLC   PRODUL,RQ_ADACC                                                  
         JNE   ACCDT150                                                         
*                                                                               
         USING GOBLOCKD,R3                                                      
         L     R3,AGOBLOCB                                                      
*                                                                               
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
*                                                                               
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US USES 1ST EXTENSION BLOCK                  
         MVC   GOABEXT,AGOBBLCK    UK USES 2ND EXTENSION BLOCK                  
*                                                                               
         MVC   GOSELCUL,CUXCPY     CPY/U/L                                      
         MVC   GOSELCUL+L'ACTKCPY(L'ACTKUNT+L'ACTKLDG),PRODUL                   
         LA    RF,RQ_ADACC+L'ACTKUNT+L'ACTKLDG                                  
         LLC   R1,PCLILEN                                                       
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   GOSELCLI(0),0(RF)                                                
         EX    R1,0(RE)                                                         
         OC    GOSELCLI,SPACES                                                  
         LA    RF,1(R1,RF)                                                      
*                                                                               
         LLC   R1,PPROLEN          SJ PRODUCT CODE                              
         LLC   R2,PCLILEN                                                       
         SR    R1,R2                                                            
         SHI   R1,1                                                             
         JM    ACCDT128                                                         
         BASR  RE,0                                                             
         CLC   0(0,RF),SPACES                                                   
         EX    R1,0(RE)                                                         
         JNH   ACCDT128                                                         
         BASR  RE,0                                                             
         MVC   GOSELPRO(0),0(RF)                                                
         EX    R1,0(RE)                                                         
         OC    GOSELPRO,SPACES                                                  
         LA    RF,1(R1,RF)                                                      
*                                                                               
         LLC   R1,PJOBLEN                                                       
         LLC   R2,PPROLEN                                                       
         SR    R1,R2                                                            
         SHI   R1,1                                                             
         JM    ACCDT128                                                         
         BASR  RE,0                                                             
         CLC   0(0,RF),SPACES                                                   
         EX    R1,0(RE)                                                         
         JNH   ACCDT128                                                         
                                                                                
         BASR  RE,0                                                             
         MVC   GOSELJOB(0),0(RF)                                                
         EX    R1,0(RE)                                                         
         OC    GOSELJOB,SPACES                                                  
ACCDT128 MVC   GOCTRY,CUCTRY                                                    
         CLI   RQ_ADMED,C' '                                                    
         JNH   *+10                                                             
         MVC   GOSELMED,RQ_ADMED                                                
         CLI   RQ_ADWC,C' '                                                     
         JNH   *+14                                                             
         MVC   GOSELWC,RQ_ADWC                                                  
         MVI   GOANYWC,YESQ                                                     
                                                                                
         GOTO1 VGETOPT,DMCB,(R3)                                                
                                                                                
         MVC   AD_CLOF,GOAUTADD                                                 
*&&UK*&& MVC   AD_BKSV,GOBILKSV        TEST KSV CAN BE BILLED                   
         MVC   AD_NAE,GONEEDAE                                                  
         MVC   AD_BILTY,GOBILTYP                                                
*&&US*&& MVI   AD_ISFO,C'N'                                                     
*&&UK                                                                           
         LLC   RF,PPROLEN                                                       
         LA    RF,RQ_ADACC+L'ACTKLDG+L'ACTKUNT(RF)                              
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         MVC   AD_ISFO,GOJULDEF                                                 
                                                                                
         LA    R3,GOTTALLW                                                      
         LA    R1,3                                                             
         MVC   AD_BILA(L'AD_BILA+L'AD_CHGA+L'AD_NONA),=C'NNN'                   
ACCDT130 CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   AD_BILA,C'Y'                                                     
         CLI   0(R3),C'R'                                                       
         JNE   *+8                                                              
         MVI   AD_CHGA,C'Y'                                                     
         CLI   0(R3),C'N'                                                       
         JNE   *+8                                                              
         MVI   AD_NONA,C'Y'                                                     
         LA    R3,1(R3)                                                         
         JCT   R1,ACCDT130                                                      
*&&                                                                             
*&&US                                                                           
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
                                                                                
         LA    R1,GOTTALLW                                                      
         LA    R0,3                                                             
         MVC   AD_BILA(L'AD_BILA+L'AD_CHGA+L'AD_NONA),=C'NNN'                   
ACCDT130 CLI   0(R1),C'B'                                                       
         JNE   *+8                                                              
         MVI   AD_BILA,C'Y'                                                     
         CLI   0(R1),C'R'                                                       
         JNE   *+8                                                              
         MVI   AD_CHGA,C'Y'                                                     
         CLI   0(R1),C'N'                                                       
         JNE   *+8                                                              
         MVI   AD_NONA,C'Y'                                                     
         LA    R1,1(R1)                                                         
         JCT   R0,ACCDT130                                                      
*&&                                                                             
         LA    R3,GOTFNARR                                                      
         LA    R1,3                                                             
         MVC   AD_FNTB(L'AD_FNTB+L'AD_FNTR+L'AD_FNTN),=C'NNN'                   
ACCDT132 CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   AD_FNTB,C'Y'                                                     
         CLI   0(R3),C'R'                                                       
         JNE   *+8                                                              
         MVI   AD_FNTR,C'Y'                                                     
         CLI   0(R3),C'N'                                                       
         JNE   *+8                                                              
         MVI   AD_FNTN,C'Y'                                                     
         LA    R3,1(R3)                                                         
         JCT   R1,ACCDT132                                                      
*                                                                               
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         MVC   AD_GAP,GOGAPS                                                    
         LLC   RF,GOGDES                                                        
         CVD   RF,DUB                                                           
         MVC   AD_DFTEX,DUB+6                                                   
         LLC   RF,GODNDV                                                        
         CVD   RF,DUB                                                           
         MVC   AD_DFTND,DUB+6                                                   
         MVC   AD_GAPAR,GOGARA                                                  
         MVC   AD_GAPEX,GOGEMX                                                  
         MVC   AD_GAPED,GOGEMD                                                  
         MVC   AD_SWPD,GOSWPD                                                   
                                                                                
         CLI   RQ_ADAPL,RQ_ADORD   ORDERS                                       
         JE    ACCDT134                                                         
         CLI   RQ_ADAPL,RQ_ADEXP   EXPENSES                                     
         JE    ACCDT134                                                         
         CLI   RQ_ADAPL,RQ_ADINV   INVOICES                                     
         JNE   ACCDT136                                                         
ACCDT134 CLI   RQ_ADBIL,NOQ                                                     
         JNE   ACCDT136                                                         
         CLI   GOBILO,YESQ                                                      
         JE    BILERR                                                           
ACCDT136 MVC   AD_BILO,GOBILO     BILLABLE ONLY                                 
         MVC   AD_CCWC,GOCCW      COLOUR CODED WORK CODES                       
         MVC   AD_FPOR,GOFPORES   Fully approved order resubmit rules           
         MVC   AD_PPOR,GOPPORES   Part approved order resubmit rules            
         MVC   AD_MPOR,GOMPORES   Matched order resubmit rules                  
         MVC   AD_WCF,GOWCF       USE WC FLAG FOR EST CHECK                     
         MVC   AD_NJLE,GONJLE     PASS ALLOW JOB LEVEL ENTRY SETTING            
         CLI   RQ_ADAPL,RQ_ADEST  Estimates                                     
         JE    *+12                                                             
         CLI   RQ_ADAPL,RQ_ADEJL  Estimates                                     
         JNE   ACCDT138                                                         
         CLI   AD_DRFT,YESQ                                                     
         JNE   ACCDT138                                                         
         CLI   GOAEDT,NOQ                                                       
         JE    DREERR                                                           
                                                                                
ACCDT138 CLI   GOTOT,C' '                                                       
         JE    *+10                                                             
         MVC   AD_DEFT,GOTOT                                                    
         CLI   RQ_ADAPL,RQ_ADTIM  ONLY FOR TIMESHEETS                           
         JNE   ACCDT146                                                         
         CLI   RQ_ADFPT,YESQ      PER2 OVERRIDE FOR PRODUCT?                    
         JNE   ACCDT140                                                         
         MVI   AD_FPT,C'A'        FORCE PRODUCT                                 
         J     ACCDT142                                                         
*                                                                               
ACCDT140 CLI   GOFPT,C' '                                                       
         JE    *+10                                                             
         MVC   AD_FPT,GOFPT                                                     
*                                                                               
ACCDT142 CLI   RQ_ADFJT,YESQ     SKIP OPT MAINT SETTING IF PER2 SET             
         JNE   ACCDT144                                                         
         MVI   AD_FJT,C'A'                                                      
         MVI   AD_JOBT,YESQ                                                     
         J     ACCDT146                                                         
*                                                                               
         USING GOBLOCKD,RF                                                      
ACCDT144 L     RF,AGOBLOCB                                                      
         MVC   AD_JOBT,GOTNOJOB                                                 
         CLI   GOFJT,C' '                                                       
         JE    *+10                                                             
         MVC   AD_FJT,GOFJT                                                     
*                                                                               
         USING GOBBLKD,RF                                                       
ACCDT146 L     RF,AGOBBLCK                                                      
         CLI   XL#JLIND,YESQ                                                    
         JNE   ACCDT148                                                         
         MVC   AD_JLE,GOJLDEST                                                  
         MVC   AD_JLO,GOJLDORD                                                  
         MVC   AD_JLT,GOJLDTSI                                                  
         MVC   AD_JLA,GOJLDADJ                                                  
         MVC   AD_JLX,GOJLDEXT                                                  
         MVC   AD_DLOCB,GOJLDBIL                                                
                                                                                
ACCDT148 CLC   AD_CURC,SPACES                                                   
         JH    ACCDT150                                                         
         CLC   GOBILCUR,AGYCURR                                                 
         JE    ACCDT150                                                         
         MVC   AD_CURC,GOBILCUR                                                 
         DROP  RF                                                               
*                                                                               
ACCDT150 MVI   AD_MDOWC,C'M'                                                    
*&&UK                                                                           
         CLI   GOICRA,GOIWCSI                                                   
         JL    *+8                                                              
*&&                                                                             
         MVI   AD_MDOWC,C'W'                                                    
         GOTOR GETMED,DMCB,RQ_ADACC,AD_MINC                                     
         JNE   EXITN                                                            
*&&UK                                                                           
         CLI   GOICRA,GOIMESK                                                   
         JNE   ACCDT200                                                         
*&&                                                                             
         MVC   AD_MINC(L'ACTKUNT+L'ACTKLDG),=C'SK'                              
         DROP  R3                                                               
                                                                                
ACCDT200 GOTOR LP_APUTO,LP_D       NOW SEND DATA                                
         J     EXITY                                                            
                                                                                
INVCLIER MVC   LP_ERROR,=AL2(AE$INCLI)                                          
         J     EXITN                                                            
INVPROER MVC   LP_ERROR,=AL2(AE$INPRO)                                          
         J     EXITN                                                            
INVJOBER MVC   LP_ERROR,=AL2(AE$INJOB)                                          
         J     EXITN                                                            
NOFUTURE MVC   LP_ERROR,=AL2(AE$NOFUT)   Invalid account future time            
         J     EXITN                                                            
TIMERR   MVC   LP_ERROR,=AL2(AE$TIMRR)   job locked from timesheets             
         J     EXITN                                                            
ORDERR   MVC   LP_ERROR,=AL2(AE$ORDRR)   job locked from orders                 
         J     EXITN                                                            
ESTERR   MVC   LP_ERROR,=AL2(AE$ESTRR)   job locked from estimates              
         J     EXITN                                                            
JOBERR   MVC   LP_ERROR,=AL2(AE$JBNAL)   job input not allowed for time         
         J     EXITN                                                            
EXPERR   MVC   LP_ERROR,=AL2(AE$EXPRR)   job locked from external costs         
         J     EXITN                                                            
BILERR   MVC   LP_ERROR,=AL2(AE$NALNB)   not allowed for non billable           
         J     EXITN                                                            
DREERR   MVC   LP_ERROR,=AL2(AE$DREST)   Draft not allowed for est              
         J     EXITN                                                            
*&&US                                                                           
JBRJERR1 MVC   LP_ERROR,=AL2(AE$JBREJ)   Job is rejected                        
         J     EXITN                                                            
JBDFERR1 MVC   LP_ERROR,=AL2(AE$JBDFT)   Job is draft                           
         J     EXITN                                                            
*&&                                                                             
         DROP  R2,R4                                                            
***********************************************************************         
* LIST OF ACCOUNT FILES TO OPEN IN ALL SYSTEMS                        *         
***********************************************************************         
                                                                                
FILES    DS    0X                  ** FILE INFO **                              
         DC    C'ACCOUNT'          SYSTEM NAME FOR OPEN                         
                                                                                
         DC    C'NCTFILE '         FILE LIST                                    
         DC    C'NGENFIL '                                                      
         DC    C'NGENDIR '                                                      
         DC    C'NACCDIR '                                                      
         DC    C'NACCMST '                                                      
         DC    C'NACCARC '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
                                                                                
FACS     DS    0X                                                               
                                                                                
***********************************************************************         
* REQUEST DEFINITIONS (REQUEST AND OUTPUT)                            *         
***********************************************************************         
                                                                                
REQUEST  DS     0X                                                              
*                                  COVERED BY LH_D DSECT - MAP HEADER           
REQACCD  DS    0XL(LH_LNQ)         ** Account Details d/load **                 
         DC    AL2(REQACCDX+1-*)                                                
         DC    AL2(A#ADTL)                                                      
         DC    AL1(0)                                                           
         DC    AL2(OUTACCD-SVRDEF)                                              
         DC    XL4'00'                                                          
EQ#ADAC  EQU   1                                                                
EC#ADAC  EQU   EQ#ADAC                                                          
         DC    AL2(EQ#ADAC)                                                     
         DC    CL5'A/C  '                                                       
         DC    AL1(EC#ADAC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADACC-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADACC)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#ACC)                                         
         DC    XL4'00'                                                          
EQ#ADLL  EQU   EQ#ADAC+1                                                        
EC#ADLL  EQU   EQ#ADLL                                                          
         DC    AL2(EQ#ADLL)                                                     
         DC    CL5'VLimL'                                                       
         DC    AL1(EC#ADLL)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADVLL-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADVLL)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#LIMLS)                                       
         DC    XL4'00'                                                          
EQ#ADLE  EQU   EQ#ADLL+1                                                        
EC#ADLE  EQU   EQ#ADLE                                                          
         DC    AL2(EQ#ADLE)                                                     
         DC    CL5'Ldgr '                                                       
         DC    AL1(EC#ADLE)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADLED-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADLED)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#LGRC)                                        
         DC    XL4'00'                                                          
EQ#ADAP  EQU   EQ#ADLE+1                                                        
EC#ADAP  EQU   EQ#ADAP                                                          
         DC    AL2(EQ#ADAP)                                                     
         DC    CL5'Apprv'                                                       
         DC    AL1(EC#ADAP)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADVAP-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADVAP)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#APRVR)                                       
         DC    XL4'00'                                                          
EQ#ADAL  EQU   EQ#ADAP+1                                                        
EC#ADAL  EQU   EQ#ADAL                                                          
         DC    AL2(EQ#ADAL)                                                     
         DC    CL5'Appl '                                                       
         DC    AL1(EC#ADAL)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADAPL-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADAPL)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#APPLI)                                       
         DC    XL4'00'                                                          
EQ#ADMC  EQU   EQ#ADAL+1                                                        
EC#ADMC  EQU   EQ#ADMC                                                          
         DC    AL2(EQ#ADMC)                                                     
         DC    CL5'MedCF'                                                       
         DC    AL1(EC#ADMC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADMED-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADMED)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#MED)                                         
         DC    XL4'00'                                                          
EQ#ADID  EQU   EQ#ADMC+1                                                        
EC#ADID  EQU   EQ#ADID                                                          
         DC    AL2(EQ#ADID)                                                     
         DC    CL5'InvDt'                                                       
         DC    AL1(EC#ADID)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADIDT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADIDT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#DATE)                                        
         DC    XL4'00'                                                          
EQ#ADSJ  EQU   EQ#ADID+1                                                        
EC#ADSJ  EQU   EQ#ADSJ                                                          
         DC    AL2(EQ#ADSJ)                                                     
         DC    CL5'C/P/J'                                                       
         DC    AL1(EC#ADSJ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADISJ-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADISJ)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#CLPJO)                                       
         DC    XL4'00'                                                          
EQ#ADOF  EQU   EQ#ADSJ+1                                                        
EC#ADOF  EQU   EQ#ADOF                                                          
         DC    AL2(EQ#ADOF)                                                     
         DC    CL5'C/P/J'                                                       
         DC    AL1(EC#ADOF)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADIOF-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADIOF)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#OFF)                                         
         DC    XL4'00'                                                          
EQ#ADFP  EQU   EQ#ADOF+1                                                        
EC#ADFP  EQU   EQ#ADFP                                                          
         DC    AL2(EQ#ADFP)                                                     
         DC    CL5'ForPr'                                                       
         DC    AL1(EC#ADFP)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADFPT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADFPT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FPT)                                         
         DC    XL4'00'                                                          
EQ#ADFJ  EQU   EQ#ADFP+1                                                        
EC#ADFJ  EQU   EQ#ADFJ                                                          
         DC    AL2(EQ#ADFJ)                                                     
         DC    CL5'ForJb'                                                       
         DC    AL1(EC#ADFJ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADFJT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADFJT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FJT)                                         
         DC    XL4'00'                                                          
EQ#ADBI  EQU   EQ#ADFJ+1                                                        
EC#ADBI  EQU   EQ#ADBI                                                          
         DC    AL2(EQ#ADBI)                                                     
         DC    CL5'Billa'                                                       
         DC    AL1(EC#ADBI)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADBIL-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADBIL)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#BLB)                                         
         DC    XL4'00'                                                          
EQ#ADFT  EQU   EQ#ADBI+1           Future time only                             
EC#ADFT  EQU   EQ#ADFT                                                          
         DC    AL2(EQ#ADFT)                                                     
         DC    CL5'Futur'                                                       
         DC    AL1(EC#ADFT)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADFTM-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADFTM)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FUTUR)                                       
         DC    XL4'00'                                                          
*                                                                               
EQ#ADEP  EQU   EQ#ADFT+1           Department                                   
EC#ADEP  EQU   EQ#ADEP                                                          
         DC    AL2(EQ#ADEP)                                                     
         DC    CL5'Depar'                                                       
         DC    AL1(EC#ADEP)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADEPT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADEPT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#DPT)                                         
         DC    XL4'00'                                                          
*                                                                               
EQ#ADWC  EQU   EQ#ADEP+1           Work code                                    
EC#ADWC  EQU   EQ#ADWC                                                          
         DC    AL2(EQ#ADWC)                                                     
         DC    CL5'Wrkcd'                                                       
         DC    AL1(EC#ADWC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADWC-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADWC)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#WC)                                          
         DC    XL4'00'                                                          
*                                                                               
EQ#ADLK  EQU   EQ#ADWC+1           Return locked from T/S account               
EC#ADLK  EQU   EQ#ADLK                                                          
         DC    AL2(EQ#ADLK)                                                     
         DC    CL5'Lkacc'                                                       
         DC    AL1(EC#ADLK)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADRLK-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADRLK)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#INLKA)                                       
         DC    XL4'00'                                                          
*                                                                               
REQACCDX DC    AL1(LD_EOTQ)                                                     
*                                                                               
REQACCL  DS    0XL(LH_LNQ)         ** account list download **                  
         DC    AL2(REQACCLX+1-*)                                                
         DC    AL2(A#ACCL)                                                      
         DC    AL1(0)                                                           
         DC    AL2(OUTACCL-SVRDEF)                                              
         DC    XL4'00'                                                          
EQ#LDGR  EQU   1                                                                
EC#LDGR  EQU   EQ#LDGR                                                          
         DC    AL2(EQ#LDGR)                                                     
         DC    CL5'Ldgr '                                                       
         DC    AL1(EC#LDGR)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALLDG-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALLDG)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#UNTLD)                                       
         DC    XL4'00'                                                          
EQ#ALCL  EQU   EQ#LDGR+1                                                        
EC#ALCL  EQU   EQ#ALCL                                                          
         DC    AL2(EQ#ALCL)                                                     
         DC    CL5'CliC '                                                       
         DC    AL1(EC#ALCL)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALCLI-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALCLI)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#OCLN)                                        
         DC    XL4'00'                                                          
EQ#ALPR  EQU   EQ#ALCL+1                                                        
EC#ALPR  EQU   EQ#ALCL                                                          
         DC    AL2(EQ#ALPR)                                                     
         DC    CL5'ProC '                                                       
         DC    AL1(EC#ALPR)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALPRO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALPRO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#PROC)                                        
         DC    XL4'00'                                                          
EQ#ALIL  EQU   EQ#ALPR+1                                                        
EC#ALIL  EQU   EQ#ALIL                                                          
         DC    AL2(EQ#ALIL)                                                     
         DC    CL5'Lock '                                                       
         DC    AL1(EC#ALIL)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALILA-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALILA)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#LCKED)                                       
         DC    XL4'00'                                                          
EQ#AAPL  EQU   EQ#ALIL+1                                                        
EC#AAPL  EQU   EQ#AAPL                                                          
         DC    AL2(EQ#AAPL)                                                     
         DC    CL5'Appl '                                                       
         DC    AL1(EC#AAPL)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALAPL-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALAPL)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#APPLI)                                       
         DC    XL4'00'                                                          
EQ#ALSU  EQU   EQ#AAPL+1                                                        
EC#ALSU  EQU   EQ#ALSU                                                          
         DC    AL2(EQ#ALSU)                                                     
         DC    CL5'SDptC'                                                       
         DC    AL1(EC#ALSU)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALSUB-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALSUB)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#SUBDP)                                       
         DC    XL4'00'                                                          
EQ#AYNO  EQU   EQ#ALSU+1                                                        
EC#AYNO  EQU   EQ#AYNO                                                          
         DC    AL2(EQ#AYNO)                                                     
         DC    CL5'Draft'                                                       
         DC    AL1(EC#AYNO)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ADYNO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ADYNO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
*&&UK*&& DC    AL1(ACCSYSQ),AL2(AC#UNAPP)                                       
*&&US*&& DC    AL1(ACCSYSQ),AL2(AC#UAPR)                                        
         DC    XL4'00'                                                          
EQ#AVAS  EQU   EQ#AYNO+1                                                        
EC#AVAS  EQU   EQ#AVAS                                                          
         DC    AL2(EQ#AVAS)                                                     
         DC    CL5'ViewA'                                                       
         DC    AL1(EC#AVAS)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALVAS-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALVAS)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#VWAL)                                        
         DC    XL4'00'                                                          
EQ#AAPP  EQU   EQ#AVAS+1                                                        
EC#AAPP  EQU   EQ#AAPP                                                          
         DC    AL2(EQ#AAPP)                                                     
         DC    CL5'Apprv'                                                       
         DC    AL1(EC#AAPP)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALAPP-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALAPP)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#APRVR)                                       
         DC    XL4'00'                                                          
EQ#AVIO  EQU   EQ#AAPP+1                                                        
EC#AVIO  EQU   EQ#AVIO                                                          
         DC    AL2(EQ#AVIO)                                                     
         DC    CL5'VatIO'                                                       
         DC    AL1(EC#AVIO)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALVIO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALVIO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
*&&UK*&& DC    AL1(ACCSYSQ),AL2(AC#VTTYP)                                       
*&&US*&& DC    AL1(ACCSYSQ),AL2(AC#VATTM)                                       
         DC    XL4'00'                                                          
EQ#AMED  EQU   EQ#AVIO+1                                                        
EC#AMED  EQU   EQ#AMED                                                          
         DC    AL2(EQ#AMED)                                                     
         DC    CL5'MedCF'                                                       
         DC    AL1(EC#AMED)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALMED-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALMED)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#MED)                                         
         DC    XL4'00'                                                          
EQ#ASOR  EQU   EQ#AMED+1                                                        
EC#ASOR  EQU   EQ#ASOR                                                          
         DC    AL2(EQ#ASOR)                                                     
         DC    CL5'SOfoR'                                                       
         DC    AL1(EC#ASOR)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALSOR-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALSOR)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#NOFF)                                        
         DC    XL4'00'                                                          
EQ#ARFN  EQU   EQ#ASOR+1                                                        
EC#ARFN  EQU   EQ#ARFN                                                          
         DC    AL2(EQ#ARFN)                                                     
         DC    CL5'ForNa'                                                       
         DC    AL1(EC#ARFN)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALRFN-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALRFN)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FDESC)                                       
         DC    XL4'00'                                                          
EQ#AFPT  EQU   EQ#ARFN+1                                                        
EC#AFPT  EQU   EQ#AFPT                                                          
         DC    AL2(EQ#AFPT)                                                     
         DC    CL5'ForPr'                                                       
         DC    AL1(EC#AFPT)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALFPT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALFPT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FPT)                                         
         DC    XL4'00'                                                          
EQ#AFJT  EQU   EQ#AFPT+1                                                        
EC#AFJT  EQU   EQ#AFJT                                                          
         DC    AL2(EQ#AFJT)                                                     
         DC    CL5'ForJb'                                                       
         DC    AL1(EC#AFJT)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALFJT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALFJT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FJT)                                         
         DC    XL4'00'                                                          
EQ#AMJO  EQU   EQ#AFJT+1           MASTER JOBS ONLY                             
EC#AMJO  EQU   EQ#AMJO                                                          
         DC    AL2(EQ#AMJO)                                                     
         DC    CL5'MasOn'                                                       
         DC    AL1(EC#AMJO)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALMJO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALMJO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#MJO)                                         
         DC    XL4'00'                                                          
EQ#ACLO  EQU   EQ#AMJO+1                                                        
EC#ACLO  EQU   EQ#ACLO             CLOSED                                       
         DC    AL2(EQ#ACLO)                                                     
         DC    CL5'Close'                                                       
         DC    AL1(EC#ACLO)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALCLO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALCLO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#CLO)                                         
         DC    XL4'00'                                                          
EQ#ALBI  EQU   EQ#ACLO+1                                                        
EC#ALBI  EQU   EQ#ALBI             Billable entry in order,expense, inv         
         DC    AL2(EQ#ALBI)                                                     
         DC    CL5'Billa'                                                       
         DC    AL1(EC#ACLO)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALBIL-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALBIL)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#BLB)                                         
         DC    XL4'00'                                                          
EQ#ALFT  EQU   EQ#ALBI+1                                                        
EC#ALFT  EQU   EQ#ALFT             Future time only                             
         DC    AL2(EQ#ALFT)                                                     
         DC    CL5'futur'                                                       
         DC    AL1(EC#ALFT)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALFTM-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALFTM)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FUTUR)                                       
         DC    XL4'00'                                                          
EQ#ALID  EQU   EQ#ALFT+1                                                        
EC#ALID  EQU   EQ#ALID                                                          
         DC    AL2(EQ#ALID)                                                     
         DC    CL5'InvDt'                                                       
         DC    AL1(EC#ALID)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALIDT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALIDT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#DATE)                                        
         DC    XL4'00'                                                          
EQ#ALWC  EQU   EQ#ALID+1                                                        
EC#ALWC  EQU   EQ#ALWC                                                          
         DC    AL2(EQ#ALWC)                                                     
         DC    CL5'WrkCd'                                                       
         DC    AL1(EC#ALWC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALWC-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALWC)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#WC)                                          
         DC    XL4'00'                                                          
*                                                                               
EQ#ALLK  EQU   EQ#ALWC+1                                                        
EC#ALLK  EQU   EQ#ALLK             Return locked from T/S account               
         DC    AL2(EQ#ALLK)                                                     
         DC    CL5'Lkacc'                                                       
         DC    AL1(EC#ALLK)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_ALRLK-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_ALRLK)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#INLKA)                                       
         DC    XL4'00'                                                          
REQACCLX DC    AL1(LD_EOTQ)                                                     
*                                                                               
REQSRCH  DS    0XL(LH_LNQ)         ** search list download **                   
         DC    AL2(REQSRCHX+1-*)                                                
         DC    AL2(A#SRCH)                                                      
         DC    AL1(0)                                                           
         DC    AL2(OUTSRCH-SVRDEF)                                              
         DC    XL4'00'                                                          
EQ#SLDG  EQU   1                                                                
EC#SLDG  EQU   EQ#SLDG                                                          
         DC    AL2(EQ#SLDG)                                                     
         DC    CL5'Ldgr '                                                       
         DC    AL1(EC#SLDG)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCUL-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCUL)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#UNTLD)                                       
         DC    XL4'00'                                                          
EQ#SLVL  EQU   EQ#SLDG+1                                                        
EC#SLVL  EQU   EQ#SLVL                                                          
         DC    AL2(EQ#SLVL)                                                     
         DC    CL5'Level'                                                       
         DC    AL1(EC#SLVL)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCLV-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCLV)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#LEVEL)                                       
         DC    XL4'00'                                                          
EQ#SOFF  EQU   EQ#SLVL+1                                                        
EC#SOFF  EQU   EQ#SOFF                                                          
         DC    AL2(EQ#SOFF)                                                     
         DC    CL5'Off  '                                                       
         DC    AL1(EC#SOFF)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCOF-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCOF)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#OFFC)                                        
         DC    XL4'00'                                                          
EQ#SRAC  EQU   EQ#SOFF+1                                                        
EC#SRAC  EQU   EQ#SRAC                                                          
         DC    AL2(EQ#SRAC)                                                     
         DC    CL5'AccPr'                                                       
         DC    AL1(EC#SRAC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCAC-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCAC)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#ACCCD)                                       
         DC    XL4'00'                                                          
EQ#SRTY  EQU   EQ#SRAC+1                                                        
EC#SRTY  EQU   EQ#SRTY                                                          
         DC    AL2(EQ#SRTY)                                                     
         DC    CL5'Type '                                                       
         DC    AL1(EC#SRTY)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCTY-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCTY)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#TYPE)                                        
         DC    XL4'00'                                                          
EQ#SRW1  EQU   EQ#SRTY+1                                                        
EC#SRW1  EQU   EQ#SRW1                                                          
         DC    AL2(EQ#SRW1)                                                     
         DC    CL5'Word1'                                                       
         DC    AL1(EC#SRW1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCW1-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCW1)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#WORD)                                        
         DC    XL4'00'                                                          
EQ#SRS1  EQU   EQ#SRW1+1                                                        
EC#SRS1  EQU   EQ#SRS1                                                          
         DC    AL2(EQ#SRS1)                                                     
         DC    CL5'Stat1'                                                       
         DC    AL1(EC#SRS1)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCS1-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCS1)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#STT)                                         
         DC    XL4'00'                                                          
EQ#SRW2  EQU   EQ#SRS1+1                                                        
EC#SRW2  EQU   EQ#SRW2                                                          
         DC    AL2(EQ#SRW2)                                                     
         DC    CL5'Word2'                                                       
         DC    AL1(EC#SRW2)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCW2-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCW2)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#WORD)                                        
         DC    XL4'00'                                                          
EQ#SRS2  EQU   EQ#SRW2+1                                                        
EC#SRS2  EQU   EQ#SRS2                                                          
         DC    AL2(EQ#SRS2)                                                     
         DC    CL5'Stat2'                                                       
         DC    AL1(EC#SRS2)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCS2-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCS2)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#STT)                                         
         DC    XL4'00'                                                          
EQ#SRW3  EQU   EQ#SRS2+1                                                        
EC#SRW3  EQU   EQ#SRW3                                                          
         DC    AL2(EQ#SRW3)                                                     
         DC    CL5'Word3'                                                       
         DC    AL1(EC#SRW3)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCW3-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCW3)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#WORD)                                        
         DC    XL4'00'                                                          
EQ#SRS3  EQU   EQ#SRW3+1                                                        
EC#SRS3  EQU   EQ#SRS3                                                          
         DC    AL2(EQ#SRS3)                                                     
         DC    CL5'Stat3'                                                       
         DC    AL1(EC#SRS3)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCS3-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCS3)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#STT)                                         
         DC    XL4'00'                                                          
EQ#SRSF  EQU   EQ#SRS3+1                                                        
EC#SRSF  EQU   EQ#SRSF                                                          
         DC    AL2(EQ#SRSF)                                                     
         DC    CL5'SecFl'                                                       
         DC    AL1(EC#SRSF)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCSF-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCSF)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#SEC)                                         
         DC    XL4'00'                                                          
EQ#SRAL  EQU   EQ#SRSF+1                                                        
EC#SRAL  EQU   EQ#SRAL                                                          
         DC    AL2(EQ#SRAL)                                                     
         DC    CL5'Appl '                                                       
         DC    AL1(EC#SRAL)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRAPL-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRAPL)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#APPLI)                                       
         DC    XL4'00'                                                          
EQ#SRDA  EQU   EQ#SRAL+1                                                        
EC#SRDA  EQU   EQ#SRDA                                                          
         DC    AL2(EQ#SRDA)                                                     
         DC    CL5'Draft'                                                       
         DC    AL1(EC#SRDA)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRYNO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRYNO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
*&&UK*&& DC    AL1(ACCSYSQ),AL2(AC#UNAPP)                                       
*&&US*&& DC    AL1(ACCSYSQ),AL2(AC#UAPR)                                        
         DC    XL4'00'                                                          
EQ#SRAP  EQU   EQ#SRDA+1                                                        
EC#SRAP  EQU   EQ#SRAP                                                          
         DC    AL2(EQ#SRAP)                                                     
         DC    CL5'Appvr'                                                       
         DC    AL1(EC#SRAP)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRAPP-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRAPP)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#APRVR)                                       
         DC    XL4'00'                                                          
EQ#SRIO  EQU   EQ#SRAP+1                                                        
EC#SRIO  EQU   EQ#SRIO                                                          
         DC    AL2(EQ#SRIO)                                                     
         DC    CL5'VATTy'                                                       
         DC    AL1(EC#SRIO)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRVIO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRVIO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
*&&UK*&& DC    AL1(ACCSYSQ),AL2(AC#VTTYP)                                       
*&&US*&& DC    AL1(ACCSYSQ),AL2(AC#VATTM)                                       
         DC    XL4'00'                                                          
EQ#SRMC  EQU   EQ#SRIO+1                                                        
EC#SRMC  EQU   EQ#SRMC                                                          
         DC    AL2(EQ#SRMC)                                                     
         DC    CL5'MedCF'                                                       
         DC    AL1(EC#SRMC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRMED-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRMED)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#MED)                                         
         DC    XL4'00'                                                          
EQ#SRDP  EQU   EQ#SRMC+1                                                        
EC#SRDP  EQU   EQ#SRDP                                                          
         DC    AL2(EQ#SRDP)                                                     
         DC    CL5'DeptF'                                                       
         DC    AL1(EC#SRDP)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRDEP-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRDEP)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#DPT)                                         
         DC    XL4'00'                                                          
EQ#SRFP  EQU   EQ#SRDP+1                                                        
EC#SRFP  EQU   EQ#SRFP                                                          
         DC    AL2(EQ#SRFP)                                                     
         DC    CL5'ForPr'                                                       
         DC    AL1(EC#SRFP)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRFPT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRFPT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FPT)                                         
         DC    XL4'00'                                                          
EQ#SRFJ  EQU   EQ#SRFP+1                                                        
EC#SRFJ  EQU   EQ#SRFJ                                                          
         DC    AL2(EQ#SRFJ)                                                     
         DC    CL5'ForJb'                                                       
         DC    AL1(EC#SRFJ)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRFJT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRFJT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FJT)                                         
         DC    XL4'00'                                                          
EQ#SRMJO EQU   EQ#SRFJ+1                                                        
EC#SRMJO EQU   EQ#SRMJO                   MASTER JOBS ONLY                      
         DC    AL2(EQ#SRMJO)                                                    
         DC    CL5'MasOn'                                                       
         DC    AL1(EC#SRMJO)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRMJO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRMJO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#MJO)                                         
         DC    XL4'00'                                                          
EQ#SRLOC EQU   EQ#SRMJO+1                                                       
EC#SRLOC EQU   EQ#SRLOC                   LOCKED ACCOUNTS                       
         DC    AL2(EQ#SRLOC)                                                    
         DC    CL5'LocAc'                                                       
         DC    AL1(EC#SRLOC)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRILA-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRILA)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#LCKED)                                       
         DC    XL4'00'                                                          
EQ#SRCLO EQU   EQ#SRLOC+1                                                       
EC#SRCLO EQU   EQ#SRCLO                   CLOSED ACCOUNTS                       
         DC    AL2(EQ#SRCLO)                                                    
         DC    CL5'CloAc'                                                       
         DC    AL1(EC#SRCLO)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRCLO-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRCLO)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#CLO)                                         
         DC    XL4'00'                                                          
EQ#SRBIL EQU   EQ#SRCLO+1                                                       
EC#SRBIL EQU   EQ#SRBIL                   Billable accounts                     
         DC    AL2(EQ#SRBIL)                                                    
         DC    CL5'Billa'                                                       
         DC    AL1(EC#SRBIL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRBIL-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRBIL)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#BLB)                                         
         DC    XL4'00'                                                          
EQ#SRFTM EQU   EQ#SRBIL+1                                                       
EC#SRFTM EQU   EQ#SRFTM                   Future time                           
         DC    AL2(EQ#SRFTM)                                                    
         DC    CL5'Futur'                                                       
         DC    AL1(EC#SRFTM)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRFTM-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRFTM)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#FUTUR)                                       
         DC    XL4'00'                                                          
EQ#SRID  EQU   EQ#SRFTM+1                                                       
EC#SRID  EQU   EQ#SRID                                                          
         DC    AL2(EQ#SRID)                                                     
         DC    CL5'InvDt'                                                       
         DC    AL1(EC#SRID)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRIDT-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRIDT)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#DATE)                                        
         DC    XL4'00'                                                          
EQ#SRWC  EQU   EQ#SRID+1                                                        
EC#SRWC  EQU   EQ#SRWC                                                          
         DC    AL2(EQ#SRWC)                                                     
         DC    CL5'WrkCd'                                                       
         DC    AL1(EC#SRWC)                                                     
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRWC-SAVED)                                               
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRWC)                                                   
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#WC)                                          
         DC    XL4'00'                                                          
EQ#SRLCK EQU   EQ#SRWC+1                                                        
EC#SRLCK EQU   EQ#SRLCK            Return locked from T/S account               
         DC    AL2(EQ#SRLCK)                                                    
         DC    CL5'Lkacc'                                                       
         DC    AL1(EC#SRLCK)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR2)                                            
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(RQ_SRLCK-SAVED)                                              
         DC    AL2(0)                                                           
         DC    AL1(L'RQ_SRLCK)                                                  
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(0,0)                                                         
         DC    XL2'00'                                                          
         DC    AL1(ACCSYSQ),AL2(AC#INLKA)                                       
         DC    XL4'00'                                                          
REQSRCHX DC    AL1(LD_EOTQ)                                                     
*                                                                               
REQUESTX DC    AL2(0)              EOT FOR REQUESTS                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT MAP FOR SECURITY DOWNLOAD FOR ACC/BRA                        *         
***********************************************************************         
** A C C O U N T   L I S T ********************************************         
OUTACCL  DS    0X                                                               
         DC    AL2(OUTACCLX-*)                                                  
         DC    AL2(R#ACCL)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(OUTACCLX-*)                                                  
         DC    AL2(E#ACCL),C'AcLst'                                             
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
ED#ACULA EQU   1                   Account code (U/L/A)                         
         DC    AL2(ED#ACULA),C'ACULA'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(AC_ULA-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_ULA)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACNAM EQU   ED#ACULA+1          Account name                                 
         DC    AL2(ED#ACNAM),C'ACNAM'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(AC_NAM-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_NAM)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACLOC EQU   ED#ACNAM+1          Locked status                                
         DC    AL2(ED#ACLOC),C'ACLOC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_LOC-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_LOC)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACCLO EQU   ED#ACLOC+1          Closed status                                
         DC    AL2(ED#ACCLO),C'ACLOC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_CLO-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_CLO)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACVAT EQU   ED#ACCLO+1          VAT rate                                     
         DC    AL2(ED#ACVAT),C'ACVAT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_VAT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_VAT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACDRA EQU   ED#ACVAT+1          Draft status                                 
         DC    AL2(ED#ACDRA),C'ACDRA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_DRA-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_DRA)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACFPT EQU   ED#ACDRA+1          force product on timesheet                   
         DC    AL2(ED#ACFPT),C'ACFPT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FPT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_FPT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACFJT EQU   ED#ACFPT+1          force job on timesheet                       
         DC    AL2(ED#ACFJT),C'ACFJT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FJT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_FJT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACBIL EQU   ED#ACFJT+1          Billable allowed                             
         DC    AL2(ED#ACBIL),C'ACBIL'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_BILA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_BILA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACCHG EQU   ED#ACBIL+1          Chargeable allowed                           
         DC    AL2(ED#ACCHG),C'ACCHG'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_CHGA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_CHGA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACNBL EQU   ED#ACCHG+1          non-billable allowed                         
         DC    AL2(ED#ACNBL),C'ACNBL'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_NBLA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_NBLA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACDEF EQU   ED#ACNBL+1          Default time type                            
         DC    AL2(ED#ACDEF),C'ACDEF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_DEFT-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_DEFT)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACOFF EQU   ED#ACDEF+1          Office on SJ                                 
         DC    AL2(ED#ACOFF),C'ACOFF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_OFFC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_OFFC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACFNB EQU   ED#ACOFF+1          force narrative on billable time             
         DC    AL2(ED#ACFNB),C'ACFNB'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FNTB-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_FNTB)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACFNR EQU   ED#ACFNB+1          force narrative on memo time                 
         DC    AL2(ED#ACFNR),C'ACFNR'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FNTR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_FNTR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACFNN EQU   ED#ACFNR+1          force narrative on non bill time             
         DC    AL2(ED#ACFNN),C'ACFNN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FNTN-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_FNTN)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACJOB EQU   ED#ACFNN+1          job input allowed on time                    
         DC    AL2(ED#ACJOB),C'ACJOB'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_JOBT-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_JOBT)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACFNM EQU   ED#ACJOB+1          Foreign name                                 
         DC    AL2(ED#ACFNM),C'ACFNM'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FNAM-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_FNAM)                                          
         DC    XL4'00'                                                          
                                                                                
ED#AC2DA EQU   ED#ACFNM+1          2D analysis                                  
         DC    AL2(ED#AC2DA),C'AC2DA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_ANAD-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_ANAD)                                          
         DC    XL4'00'                                                          
                                                                                
ED#AC2PA EQU   ED#AC2DA+1          2P analysis                                  
         DC    AL2(ED#AC2PA),C'AC2PA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_ANAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_ANAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACLB  EQU   ED#AC2PA+1          LOCKED FROM BILLING                          
         DC    AL2(ED#ACLB),C'ACDLB'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_DLOCB-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_DLOCB)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACOFN EQU   ED#ACLB+1           Office name on SJ                            
         DC    AL2(ED#ACOFN),C'ACOFN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_OFFN-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_OFFN)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACNJL EQU   ED#ACOFN+1          No job level entry                           
         DC    AL2(ED#ACNJL),C'ACNJL'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_NJLE-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_NJLE)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACCST EQU   ED#ACNJL+1           Costing group                               
         DC    AL2(ED#ACCST),C'ACCST'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_CSTG-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_CSTG)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACBIO EQU   ED#ACCST+1          Billable only                                
         DC    AL2(ED#ACBIO),C'ACBIO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_BILO-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_BILO)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACMJO EQU   ED#ACBIO+1          Master job                                   
         DC    AL2(ED#ACMJO),C'ACMJO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_MJO-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_MJO)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACSDR EQU   ED#ACMJO+1          Supplier discount rate                       
         DC    AL2(ED#ACSDR),C'ACSDR'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_DISC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_DISC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACMJC EQU   ED#ACSDR+1          Master job code if sub job                   
         DC    AL2(ED#ACMJC),C'ACMJC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_MJC-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_MJC)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACFUT EQU   ED#ACMJC+1          Future time allowed on 1N account            
         DC    AL2(ED#ACFUT),C'ACFUT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FUTA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_FUTA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACMWC EQU   ED#ACFUT+1          Media or workcode suspense income ac         
         DC    AL2(ED#ACMWC),C'ACMWC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_MDOWC-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_MDOWC)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACMIN EQU   ED#ACMWC+1          Media suspense income ac                     
         DC    AL2(ED#ACMIN),C'ACMIN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_MINC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_MINC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACAD1 EQU   ED#ACMIN+1          Account address line 1                       
         DC    AL2(ED#ACAD1),C'ACAD1'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_ADDR1-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_ADDR1)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAD2 EQU   ED#ACAD1+1          Account address line 2                       
         DC    AL2(ED#ACAD2),C'ACAD2'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_ADDR2-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_ADDR2)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAD3 EQU   ED#ACAD2+1          Account address line 3                       
         DC    AL2(ED#ACAD3),C'ACAD3'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_ADDR3-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_ADDR3)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAD4 EQU   ED#ACAD3+1          Account address line 4                       
         DC    AL2(ED#ACAD4),C'ACAD4'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_ADDR4-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_ADDR4)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAGC EQU   ED#ACAD4+1          Agent account code                           
         DC    AL2(ED#ACAGC),C'ACAGC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_AGACC-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_AGACC)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAGN EQU   ED#ACAGC+1          Agent account name                           
         DC    AL2(ED#ACAGC),C'ACAGN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_AGNAM-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_AGNAM)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAG1 EQU   ED#ACAGN+1          Agent account address line 1                 
         DC    AL2(ED#ACAG1),C'ACAG1'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_AGAD2-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_AGAD2)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAG2 EQU   ED#ACAG1+1          Agent account address line 2                 
         DC    AL2(ED#ACAG2),C'ACAG2'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_AGAD2-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_AGAD2)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAG3 EQU   ED#ACAG2+1          Agent account address line 3                 
         DC    AL2(ED#ACAG3),C'ACAG3'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_AGAD3-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_AGAD3)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACAG4 EQU   ED#ACAG3+1          Agent account address line 4                 
         DC    AL2(ED#ACAG4),C'ACAG4'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_AGAD4-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_AGAD4)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACCUR EQU   ED#ACAG4+1          Currency Code                                
         DC    AL2(ED#ACCUR),C'ACCUR'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_CUR-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_CUR)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACNAE EQU   ED#ACCUR+1          Need approved estimate                       
         DC    AL2(ED#ACNAE),C'ACNAE'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_NAE-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_NAE)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACVAC EQU   ED#ACNAE+1          VAT code                                     
         DC    AL2(ED#ACVAC),C'ACVAC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_VATC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_VATC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACKSV EQU   ED#ACVAC+1          KSV rate                                     
         DC    AL2(ED#ACKSV),C'ACKSV'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_KSVR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_KSVR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#AC13V EQU   ED#ACKSV+1          13b VAT indicator                            
         DC    AL2(ED#AC13V),C'AC13V'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_13BVI-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_13BVI)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACBKS EQU   ED#AC13V+1          Billable KSV                                 
         DC    AL2(ED#ACBKS),C'ACBKS'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_BKSV-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_BKSV)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACMIA EQU   ED#ACBKS+1          Mileage analysis                             
         DC    AL2(ED#ACMIA),C'ACMIA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_ANAM-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_ANAM)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACIFO EQU   ED#ACMIA+1          Is foreign language user                     
         DC    AL2(ED#ACIFO),C'ACIFO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_ISFO-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_ISFO)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACDDF EQU   ED#ACIFO+1          Invoices: Due date formula                   
         DC    AL2(ED#ACDDF),C'ACDDF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_DDXP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_DDXP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACGAP EQU   ED#ACDDF+1          GAP service in use                           
         DC    AL2(ED#ACGAP),C'GAP  '                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_GAP-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_GAP)                                           
         DC    XL4'00'                                                          
*                                                                               
ED#ACDFX EQU   ED#ACGAP+1          Default expiration period                    
         DC    AL2(ED#ACDFX),C'DefEx'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_DFTEX-SAVED)                                              
         DC    AL1(LD_SPAKQ,L'AC_DFTEX)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#ACDTV EQU   ED#ACDFX+1          Default no. of days to view estimate         
         DC    AL2(ED#ACDTV),C'DefNd'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_DFTND-SAVED)                                              
         DC    AL1(LD_SPAKQ,L'AC_DFTND)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACGEX EQU   ED#ACDTV+1          GAP estimate email extension                 
         DC    AL2(ED#ACGEX),C'GAPEX'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_GAPEX-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_GAPEX)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#ACGAR EQU   ED#ACGEX+1          GAP Approval required by all                 
         DC    AL2(ED#ACGAR),C'GAPAr'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_GAPAR-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_GAPAR)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#ACEML EQU   ED#ACGAR+1          Email address                                
         DC    AL2(ED#ACEML),C'Email'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_EMADD-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_EMADD)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#ACBTY EQU   ED#ACEML+1          Billing type                                 
         DC    AL2(ED#ACBTY),C'BilTy'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_BILTY-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_BILTY)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACSGA EQU   ED#ACBTY+1          Supplier GAP setting                         
         DC    AL2(ED#ACSGA),C'SupGa'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_SGAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_SGAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACSAQ EQU   ED#ACSGA+1          Supplier Acknowledge/Query                   
         DC    AL2(ED#ACSAQ),C'AckQu'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_AQAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_AQAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACSWQ EQU   ED#ACSAQ+1          Suppress W/c printing default (est)          
         DC    AL2(ED#ACSWQ),C'Swpdf'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_SWPD-SAVED)                                               
         DC    AL1(LD_HEXDQ,L'AC_SWPD)                                          
         DC    XL4'00'                                                          
                                                                                
ED#AFAXQ EQU   ED#ACSWQ+1          Fax number                                   
         DC    AL2(ED#AFAXQ),C'FaxNo'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FAX-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_FAX)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ATELQ EQU   ED#AFAXQ+1          Telephone number                             
         DC    AL2(ED#ATELQ),C'Telep'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_TELE-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_TELE)                                          
         DC    XL4'00'                                                          
                                                                                
ED#GAPED EQU   ED#ATELQ+1          GAP estimates email disclaimer               
         DC    AL2(ED#GAPED),C'GAPED'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_GAPED-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_GAPED)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACXAP EQU   ED#GAPED+1          Expense approvers are chosen                 
         DC    AL2(ED#ACXAP),C'ExpAp'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_XAPP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_XAPP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACMIL EQU   ED#ACXAP+1          Expense new mileage rates in use             
         DC    AL2(ED#ACMIL),C'ExMil'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_XMIL-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_XMIL)                                          
         DC    XL4'00'                                                          
                                                                                
ED#AC1RO EQU   ED#ACMIL+1          1R office code                               
         DC    AL2(ED#AC1RO),C'1ROfc'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_1ROFF-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_1ROFF)                                         
         DC    XL4'00'                                                          
                                                                                
ED#AC1RD EQU   ED#AC1RO+1          1R department code                           
         DC    AL2(ED#AC1RD),C'1RDpt'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_1RDPT-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_1RDPT)                                         
         DC    XL4'00'                                                          
                                                                                
ED#AC1RS EQU   ED#AC1RD+1          1R Sub department code                       
         DC    AL2(ED#AC1RS),C'1RSub'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_1RSUB-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AC_1RSUB)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ACCCW EQU   ED#AC1RS+1          Colour coded work codes                      
         DC    AL2(ED#ACCCW),C'ACCCW'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_CCWC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_CCWC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACWCF EQU   ED#ACCCW+1          Work code flag for estimate check            
         DC    AL2(ED#ACWCF),C'ACWCF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_WCF-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AC_WCF)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ACPPO EQU   ED#ACWCF+1          Part approved orders resubmit                
         DC    AL2(ED#ACPPO),C'ACPPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_PPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_PPOR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACFPO EQU   ED#ACPPO+1          Fully approved orders resubmit               
         DC    AL2(ED#ACFPO),C'ACFPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_FPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_FPOR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ACMPO EQU   ED#ACFPO+1          Matched approved orders resubmit             
         DC    AL2(ED#ACMPO),C'ACMPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AC_MPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_MPOR)                                          
         DC    XL4'00'                                                          
                                                                                
OUTACCLX DS    0X                                                               
                                                                                
** S E A R C H    L I S T *********************************************         
                                                                                
OUTSRCH  DS    0X                                                               
         DC    AL2(OUTSRCHX-*)                                                  
         DC    AL2(R#SRCH)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(OUTSRCHX-*)                                                  
         DC    AL2(E#SRCH),C'ASrch'                                             
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
ED#SRULA EQU   1                   Account's U/L/A                              
         DC    AL2(ED#SRULA),C'SRULA'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(SR_ULA-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_ULA)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRNAM EQU   ED#SRULA+1          Account name                                 
         DC    AL2(ED#SRNAM),C'SRNAM'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(SR_NAM-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_NAM)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRAGA EQU   ED#SRNAM+1          Agent's account                              
         DC    AL2(ED#SRAGA),C'SRAGA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_AGA-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_AGA)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRAGN EQU   ED#SRAGA+1          Agent's name                                 
         DC    AL2(ED#SRAGN),C'SRAGN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_AGN-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_AGN)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRVAT EQU   ED#SRAGN+1          VAT rate on SG                               
         DC    AL2(ED#SRVAT),C'SRVAT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_VAT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_VAT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRDRA EQU   ED#SRVAT+1          Draft status                                 
         DC    AL2(ED#SRDRA),C'SRDRA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_DRA-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_DRA)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRFPT EQU   ED#SRDRA+1          force product on timesheet                   
         DC    AL2(ED#SRFPT),C'SRFPT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FPT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_FPT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRFJT EQU   ED#SRFPT+1          force job on timesheet                       
         DC    AL2(ED#SRFJT),C'SRFJT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FJT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_FJT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRBIL EQU   ED#SRFJT+1          Billable allowed                             
         DC    AL2(ED#SRBIL),C'SRBIL'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_BILA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_BILA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRCHG EQU   ED#SRBIL+1          Chargeable allowed                           
         DC    AL2(ED#SRCHG),C'SRCHG'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_CHGA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_CHGA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRNBL EQU   ED#SRCHG+1          Non billable allowed                         
         DC    AL2(ED#SRNBL),C'SRNBL'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_NBLA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_NBLA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRDEF EQU   ED#ACNBL+1          Default time type                            
         DC    AL2(ED#SRDEF),C'SRDEF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_DEFT-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_DEFT)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRFNB EQU   ED#SRDEF+1          Force narrative on bill time                 
         DC    AL2(ED#SRFNB),C'SRFNB'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FNTB-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_FNTB)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRFNR EQU   ED#SRFNB+1          Force narrative on memo time                 
         DC    AL2(ED#SRFNR),C'SRFNR'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FNTR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_FNTR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRFNN EQU   ED#SRFNR+1          Force narrative on non bill time             
         DC    AL2(ED#SRFNN),C'SRFNN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FNTN-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_FNTN)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SROFF EQU   ED#SRFNN+1          SJ office code                               
         DC    AL2(ED#SROFF),C'SROFF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_OFFC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_OFFC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRJOB EQU   ED#SROFF+1          Job input allowed in time                    
         DC    AL2(ED#SRJOB),C'SRJOB'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_JOBT-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_JOBT)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SR2DA EQU   ED#SRJOB+1          2D analysis                                  
         DC    AL2(ED#SR2DA),C'SR2DA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_ANAD-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_ANAD)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SR2PA EQU   ED#SR2DA+1          2P analysis                                  
         DC    AL2(ED#SR2PA),C'SR2PA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_ANAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_ANAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRLB  EQU   ED#SR2PA+1          LOCK FROM BILLING                            
         DC    AL2(ED#SRLB),C'SRLBL'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_DLOCB-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_DLOCB)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#SRON  EQU   ED#SRLB+1           OFFICE NAME (SJ)                             
         DC    AL2(ED#SRON),C'SROFN'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_OFFN-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_OFFN)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRCC  EQU   ED#SRON+1           CLIENT CODE                                  
         DC    AL2(ED#SRCC),C'SRCLC'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_CLI-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_CLI)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRCN  EQU   ED#SRCC+1           CLIENT NAME                                  
         DC    AL2(ED#SRCN),C'SRCLN'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_CLINM-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_CLINM)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRPC  EQU   ED#SRCN+1           PRODUCT CODE                                 
         DC    AL2(ED#SRPC),C'SRPRC'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_PRO-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_PRO)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRPN  EQU   ED#SRPC+1           PRODUCT NAME                                 
         DC    AL2(ED#SRPN),C'SRPRN'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_PRONM-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_PRONM)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRNJ  EQU   ED#SRPN+1           NO JOB LEVEL ENTRY FOR EXPENSES              
         DC    AL2(ED#SRNJ),C'SRNJL'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_NJLE-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_NJLE)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRCST EQU   ED#SRNJ+1           Costing group                                
         DC    AL2(ED#SRCST),C'SRCST'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_CSTG-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_CSTG)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRBO  EQU   ED#SRCST+1           BILLABLE ONLY                               
         DC    AL2(ED#SRBO),C'SRBIO'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_BILO-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_BILO)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRMJO EQU   ED#SRBO+1           MASTER JOB                                   
         DC    AL2(ED#SRMJO),C'SRMJO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_MJO-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_MJO)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRLOC EQU   ED#SRMJO+1          LOCKED JOB                                   
         DC    AL2(ED#SRLOC),C'SRLOC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_LOC-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_LOC)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRCLO EQU   ED#SRLOC+1          CLOSED JOB                                   
         DC    AL2(ED#SRCLO),C'SRCLO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_CLO-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_CLO)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRSDR EQU   ED#SRCLO+1          SUPPLIER DISCOUNT RATE                       
         DC    AL2(ED#SRSDR),C'SRSDR'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_DISC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_DISC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#1ROC  EQU   ED#SRSDR+1          1R OFFICE CODE                               
         DC    AL2(ED#1ROC),C'1ROFC'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_1ROFC-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_1ROFC)                                         
         DC    XL4'00'                                                          
                                                                                
ED#1RON  EQU   ED#1ROC+1           1R OFFICE NAME                               
         DC    AL2(ED#1RON),C'1ROFN'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_1ROFN-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_1ROFN)                                         
         DC    XL4'00'                                                          
                                                                                
ED#1RDC  EQU   ED#1RON+1           1R DEPARTMENT CODE                           
         DC    AL2(ED#1RDC),C'1RDEC'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_1RDEC-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_1RDEC)                                         
         DC    XL4'00'                                                          
                                                                                
ED#1RDN  EQU   ED#1RDC+1           1R DEPARTMENT NAME                           
         DC    AL2(ED#1RDN),C'1RDEN'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_1RDEN-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_1RDEN)                                         
         DC    XL4'00'                                                          
                                                                                
ED#1RSC  EQU   ED#1RDN+1           1R SUB DEPARTMENT CODE                       
         DC    AL2(ED#1RSC),C'1RSDC'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_1RSDC-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_1RSDC)                                         
         DC    XL4'00'                                                          
                                                                                
ED#1RSN  EQU   ED#1RSC+1           1R SUB DEPARTMENT NAME                       
         DC    AL2(ED#1RSN),C'1RSDN'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_1RSDN-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_1RSDN)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRMJC EQU   ED#1RSN+1           MASTER JOB IF SUB JOB                        
         DC    AL2(ED#SRMJC),C'SRMJC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_MJC-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_MJC)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRFUT EQU   ED#SRMJC+1          FUTURE TIME ALLOWED                          
         DC    AL2(ED#SRFUT),C'SRFUT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FUTA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_FUTA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRMWC EQU   ED#SRFUT+1          Media or workcode suspense income ac         
         DC    AL2(ED#SRMWC),C'SRMWC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_MDOWC-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_MDOWC)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRMIN EQU   ED#SRMWC+1          Media suspense income ac                     
         DC    AL2(ED#SRMIN),C'SRMIN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_MINC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_MINC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRAD1 EQU   ED#SRMIN+1          Account address line 1                       
         DC    AL2(ED#SRAD1),C'SRAD1'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_ADDR1-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_ADDR1)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRAD2 EQU   ED#SRAD1+1          Account address line 2                       
         DC    AL2(ED#SRAD2),C'SRAD2'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_ADDR2-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_ADDR2)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRAD3 EQU   ED#SRAD2+1          Account address line 3                       
         DC    AL2(ED#SRAD3),C'SRAD3'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_ADDR3-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_ADDR3)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRAD4 EQU   ED#SRAD3+1          Account address line 4                       
         DC    AL2(ED#SRAD4),C'SRAD4'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_ADDR4-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_ADDR4)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRAG1 EQU   ED#SRAD4+1          Agent account address line 1                 
         DC    AL2(ED#SRAG1),C'SRAG1'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_AGAD1-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_AGAD1)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRAG2 EQU   ED#SRAG1+1          Agent account address line 2                 
         DC    AL2(ED#SRAG2),C'SRAG2'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_AGAD2-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_AGAD2)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRAG3 EQU   ED#SRAG2+1          Agent account address line 3                 
         DC    AL2(ED#SRAG3),C'SRAG3'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_AGAD3-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_AGAD3)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRAG4 EQU   ED#SRAG3+1          Agent account address line 4                 
         DC    AL2(ED#SRAG4),C'SRAG4'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_AGAD4-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_AGAD4)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRCUR EQU   ED#SRAG4+1          Currency Code                                
         DC    AL2(ED#SRCUR),C'SRCUR'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_CUR-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_CUR)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRNAE EQU   ED#SRCUR+1          Need approved estimate                       
         DC    AL2(ED#SRNAE),C'SRNAE'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_NAE-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_NAE)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRVAC EQU   ED#SRNAE+1          VAT code                                     
         DC    AL2(ED#SRVAC),C'SRVAC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_VATC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_VATC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRKSV EQU   ED#SRVAC+1          KSV rate                                     
         DC    AL2(ED#SRKSV),C'SRKSV'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_KSVR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_KSVR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SR13V EQU   ED#SRKSV+1          13b VAT indicator                            
         DC    AL2(ED#SR13V),C'SR13V'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_13BVI-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_13BVI)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRBKS EQU   ED#SR13V+1          Billable KSV                                 
         DC    AL2(ED#SRBKS),C'SRBKS'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_BKSV-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_BKSV)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRMIA EQU   ED#SRBKS+1          Mileage analysis                             
         DC    AL2(ED#SRMIA),C'SRMIA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_ANAM-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_ANAM)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRIFO EQU   ED#SRMIA+1          Is foreign language user                     
         DC    AL2(ED#SRIFO),C'SRIFO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_ISFO-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_ISFO)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRDDF EQU   ED#SRIFO+1          Invoices: Due date formula                   
         DC    AL2(ED#SRDDF),C'SRDDF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_DDXP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_DDXP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRFNA EQU   ED#SRDDF+1          Foreign name                                 
         DC    AL2(ED#SRFNA),C'SRFNA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FNAM-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_FNAM)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRGAP EQU   ED#SRFNA+1          GAP service in use                           
         DC    AL2(ED#SRGAP),C'GAP  '                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_GAP-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_GAP)                                           
         DC    XL4'00'                                                          
*                                                                               
ED#SRDFX EQU   ED#SRGAP+1          Default expiration period                    
         DC    AL2(ED#SRDFX),C'DefEx'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_DFTEX-SAVED)                                              
         DC    AL1(LD_SPAKQ,L'SR_DFTEX)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#SRDTV EQU   ED#SRDFX+1          Default no. of days to view estimate         
         DC    AL2(ED#SRDTV),C'DefNd'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_DFTND-SAVED)                                              
         DC    AL1(LD_SPAKQ,L'SR_DFTND)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRGEX EQU   ED#SRDTV+1          GAP estimate email extension                 
         DC    AL2(ED#SRGEX),C'GAPEX'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_GAPEX-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_GAPEX)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#SRGAR EQU   ED#SRGEX+1          GAP Approval required by all                 
         DC    AL2(ED#SRGAR),C'GAPAr'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_GAPAR-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_GAPAR)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#SREML EQU   ED#SRGAR+1          Email address                                
         DC    AL2(ED#SREML),C'Email'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_EMADD-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_EMADD)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#SRBTY EQU   ED#SREML+1          Billing type                                 
         DC    AL2(ED#SRBTY),C'BilTy'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_BILTY-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_BILTY)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRSGA EQU   ED#SRBTY+1          Supplier GAP service                         
         DC    AL2(ED#SRSGA),C'SupGa'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_SGAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_SGAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRSAQ EQU   ED#SRSGA+1          Supplier Acknowledge Query                   
         DC    AL2(ED#SRSAQ),C'AckQu'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_AQAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_AQAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SWCPQ EQU   ED#SRSAQ+1          Suppress W/C printing default (est)          
         DC    AL2(ED#SWCPQ),C'Swcpd'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_SWPD-SAVED)                                               
         DC    AL1(LD_HEXDQ,L'SR_SWPD)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SFAXQ EQU   ED#SWCPQ+1          Fax number                                   
         DC    AL2(ED#SFAXQ),C'FaxNo'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FAX-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_FAX)                                           
         DC    XL4'00'                                                          
                                                                                
ED#STELQ EQU   ED#SFAXQ+1          Telephone number                             
         DC    AL2(ED#STELQ),C'Telep'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_TELE-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_TELE)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRGED EQU   ED#STELQ+1          GAP estimates email disclaimer               
         DC    AL2(ED#SRGED),C'GAPED'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_GAPED-SAVED)                                              
         DC    AL1(LD_CHARQ,L'SR_GAPED)                                         
         DC    XL4'00'                                                          
                                                                                
ED#SRXAP EQU   ED#SRGED+1          Expense Approvers are chosen                 
         DC    AL2(ED#SRXAP),C'ExpAp'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_XAPP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_XAPP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRMIL EQU   ED#SRXAP+1          Expenses new mileage rates in use            
         DC    AL2(ED#SRMIL),C'ExMil'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_XMIL-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_XMIL)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRCCW EQU   ED#SRMIL+1           COLOUR CODED WORK CODES                     
         DC    AL2(ED#SRCCW),C'SRCCW'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_CCWC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_CCWC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRWCF EQU   ED#SRCCW+1           WORKCODE FLAG FOR ESTIMATE CHECK            
         DC    AL2(ED#SRWCF),C'SRWCF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_WCF-SAVED)                                                
         DC    AL1(LD_CHARQ,L'SR_WCF)                                           
         DC    XL4'00'                                                          
                                                                                
ED#SRPPO EQU   ED#SRWCF+1          Part approved orders resubmit                
         DC    AL2(ED#SRPPO),C'SRPPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_PPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_PPOR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRFPO EQU   ED#SRPPO+1          Fully approved orders resubmit               
         DC    AL2(ED#SRFPO),C'SRFPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_FPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_FPOR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SRMPO EQU   ED#SRFPO+1          Matched approved orders resubmit             
         DC    AL2(ED#SRMPO),C'SRMPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(SR_MPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'SR_MPOR)                                          
         DC    XL4'00'                                                          
*                                                                               
OUTSRCHX DS    0X                                                               
*                                                                               
** A C C O U N T   D E T A I L S **************************************         
*                                                                               
OUTACCD  DS    0X                                                               
         DC    AL2(OUTACCDX-*)                                                  
         DC    AL2(R#ADTL)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
         DC    AL2(OUTACCDX-*)                                                  
         DC    AL2(E#ADTL),C'AccDt'                                             
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
                                                                                
ED#ADNAM EQU   1                   Name                                         
         DC    AL2(ED#ADNAM),C'ADNAM'                                           
         DC    AL1(B#SAVED,0,0)                                                 
         DC    AL2(AD_NAME-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_NAME)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADAD1 EQU   ED#ADNAM+1          Address 1                                    
         DC    AL2(ED#ADAD1),C'ADAD1'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_ADD1-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_ADD1)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADAD2 EQU   ED#ADAD1+1          Address 2                                    
         DC    AL2(ED#ADAD2),C'ADAD2'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_ADD2-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_ADD2)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADAD3 EQU   ED#ADAD2+1          Address 3                                    
         DC    AL2(ED#ADAD3),C'ADAD3'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_ADD3-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_ADD3)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADAD4 EQU   ED#ADAD3+1          Address 4                                    
         DC    AL2(ED#ADAD4),C'ADAD4'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_ADD4-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_ADD4)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADCUR EQU   ED#ADAD4+1          Currency Code                                
         DC    AL2(ED#ADCUR),C'ADCUR'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_CURC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_CURC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADBIL EQU   ED#ADCUR+1          Billable time allowed - SJ only              
         DC    AL2(ED#ADBIL),C'ADBIL'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_BILA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_BILA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADCHG EQU   ED#ADBIL+1          Chargeable time allowed - SJ only            
         DC    AL2(ED#ADCHG),C'ADCHG'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_CHGA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_CHGA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADNON EQU   ED#ADCHG+1          Non-billable time allowed - SJ only          
         DC    AL2(ED#ADNON),C'ADNON'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_NONA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_NONA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADDEF EQU   ED#ADNON+1          Default time type - SJ only                  
         DC    AL2(ED#ADDEF),C'ADDEF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_DEFT-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_DEFT)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADSJO EQU   ED#ADDEF+1          SJ office code                               
         DC    AL2(ED#ADSJO),C'ADSJO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_OFFC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_OFFC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADDRA EQU   ED#ADSJO+1          Draft status                                 
         DC    AL2(ED#ADDRA),C'ADDRA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_DRFT-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_DRFT)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADIFO EQU   ED#ADDRA+1          Is foreign language user                     
         DC    AL2(ED#ADIFO),C'ADIFO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_ISFO-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_ISFO)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADFNA EQU   ED#ADIFO+1          Foreign name                                 
         DC    AL2(ED#ADFNA),C'ADFNA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FNAM-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_FNAM)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADFPT EQU   ED#ADFNA+1          Force product on timesheet                   
         DC    AL2(ED#ADFPT),C'ADFPT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FPT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_FPT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADFJT EQU   ED#ADFPT+1          Force job on timesheet                       
         DC    AL2(ED#ADFJT),C'ADFJT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FJT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_FJT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADBOE EQU   ED#ADFJT+1          BramndOcean estimate user/job                
         DC    AL2(ED#ADBOE),C'ADBOE'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_BOE-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_BOE)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADFNB EQU   ED#ADBOE+1          BrandOcean force narrative on                
         DC    AL2(ED#ADFNB),C'ADFNB'             billable time                 
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FNTB-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_FNTB)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADFNR EQU   ED#ADFNB+1          BrandOcean force narrative on                
         DC    AL2(ED#ADFNR),C'ADFNR'            memo time                      
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FNTR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_FNTR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADFNN EQU   ED#ADFNR+1          BrandOcean force narrative on                
         DC    AL2(ED#ADFNN),C'ADFNN'         non billable time                 
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FNTN-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_FNTN)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADCLO EQU   ED#ADFNN+1          Close status                                 
         DC    AL2(ED#ADCLO),C'ADCLO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_CLOS-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_CLOS)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADVAT EQU   ED#ADCLO+1          VAT rate on SG                               
         DC    AL2(ED#ADVAT),C'ADVAT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_VATR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_VATR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADJOB EQU   ED#ADVAT+1          Job input allowed on time                    
         DC    AL2(ED#ADJOB),C'ADJOB'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_JOBT-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_JOBT)                                          
         DC    XL4'00'                                                          
                                                                                
ED#AD2DA EQU   ED#ADJOB+1          2D analysis                                  
         DC    AL2(ED#AD2DA),C'AD2DA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_ANAD-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_ANAD)                                          
         DC    XL4'00'                                                          
                                                                                
ED#AD2PA EQU   ED#AD2DA+1          2P analysis                                  
         DC    AL2(ED#AD2PA),C'AD2PA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_ANAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_ANAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADVAC EQU   ED#AD2PA+1          VAT code                                     
         DC    AL2(ED#ADVAC),C'ADVAC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_VATC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_VATC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADKSV EQU   ED#ADVAC+1          KSV rate                                     
         DC    AL2(ED#ADKSV),C'ADKSV'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_KSVR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_KSVR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADDSC EQU   ED#ADKSV+1          Discount                                     
         DC    AL2(ED#ADDSC),C'ADDSC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_DISC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_DISC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADDDF EQU   ED#ADDSC+2          Invoices: Due date formula                   
         DC    AL2(ED#ADDDF),C'ADDDF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_DDXP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_DDXP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADLB  EQU   ED#ADDDF+1          LOCKED FROM BILLING                          
         DC    AL2(ED#ADLB),C'ADLBI'                                            
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_DLOCB-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_DLOCB)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADOFN EQU   ED#ADLB+1           OFFICE NAME (SJ ONLY)                        
         DC    AL2(ED#ADOFN),C'ADONA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_OFFN-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_OFFN)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADJNE EQU   ED#ADOFN+1           Allow job level entry for expenses          
         DC    AL2(ED#ADJNE),C'ADNJL'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_NJLE-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_NJLE)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADCST EQU   ED#ADJNE+1           Costing group                               
         DC    AL2(ED#ADCST),C'ADCST'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_CSTG-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_CSTG)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADBIO EQU   ED#ADCST+1           Billable only                               
         DC    AL2(ED#ADBIO),C'ADBIO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_BILO-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_BILO)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADMJO EQU   ED#ADBIO+1           Master job                                  
         DC    AL2(ED#ADMJO),C'ADMJO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_MJO-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_MJO)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADLOC EQU   ED#ADMJO+1           Locked job                                  
         DC    AL2(ED#ADLOC),C'ADLOC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_LOC-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_LOC)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADJLE EQU   38                   Job locks estimate                          
         DC    AL2(ED#ADJLE),C'ADJLE'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_JLE-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_JLE)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADJLO EQU   39                   Job locks orders                            
         DC    AL2(ED#ADJLO),C'ADJLO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_JLO-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_JLO)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADJLT EQU   40                   Job locks timesheets                        
         DC    AL2(ED#ADJLT),C'ADJLT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_JLT-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_JLT)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADJLA EQU   41                   Job locks adjustments                       
         DC    AL2(ED#ADJLA),C'ADJLA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_JLA-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_JLA)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADJLX EQU   42                   Job locks externals                         
         DC    AL2(ED#ADJLX),C'ADJLX'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_JLX-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_JLX)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADMJC EQU   43                   Master job code is a sub job                
         DC    AL2(ED#ADMJC),C'ADMJC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_MJC-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_MJC)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADCLD EQU   44                   Close date offset                           
         DC    AL2(ED#ADCLD),C'ADCLO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_CLOF-SAVED)                                               
         DC    AL1(LD_CBINQ,L'AD_CLOF)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADAGC EQU   45                   Artist agent code                           
         DC    AL2(ED#ADAGC),C'ADAGC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_AGAC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_AGAC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADAGN EQU   46                   Artist agent name                           
         DC    AL2(ED#ADAGN),C'ADAGN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_AGNAM-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_AGNAM)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADAG1 EQU   47                   Artist agent address line 1                 
         DC    AL2(ED#ADAG1),C'ADAG1'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_AGAD1-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_AGAD1)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADAG2 EQU   48                   Artist agent address line 2                 
         DC    AL2(ED#ADAG2),C'ADAG2'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_AGAD2-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_AGAD2)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADAG3 EQU   49                   Artist agent address line 3                 
         DC    AL2(ED#ADAG3),C'ADAG3'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_AGAD3-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_AGAD3)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADAG4 EQU   50                   Artist agent address line 4                 
         DC    AL2(ED#ADAG4),C'ADAG4'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_AGAD4-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_AGAD4)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADFUT EQU   51                   Future time allowed                         
         DC    AL2(ED#ADFUT),C'ADFUT'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FUTA-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_FUTA)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADMWC EQU   ED#ADFUT+1          Media or workcode suspense income ac         
         DC    AL2(ED#ADMWC),C'ADMWC'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_MDOWC-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_MDOWC)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADMIN EQU   ED#ADMWC+1          Media suspense income ac                     
         DC    AL2(ED#ADMIN),C'ADMIN'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_MINC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_MINC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADNAE EQU   ED#ADMIN+1          Need approved estimate                       
         DC    AL2(ED#ADNAE),C'ADNAE'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_NAE-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_NAE)                                           
         DC    XL4'00'                                                          
                                                                                
ED#AD13V EQU   ED#ADNAE+1          13b VAT indicator                            
         DC    AL2(ED#AD13V),C'AD13V'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_13BVI-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_13BVI)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADBKS EQU   ED#AD13V+1          Billable KSV                                 
         DC    AL2(ED#ADBKS),C'ADBKS'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_BKSV-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_BKSV)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADMIA EQU   ED#ADBKS+1          Mileage analysis                             
         DC    AL2(ED#ADMIA),C'ADMIA'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_ANAM-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AC_ANAM)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADGAP EQU   ED#ADMIA+1          GAP service in use                           
         DC    AL2(ED#ADGAP),C'GAP  '                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_GAP-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_GAP)                                           
         DC    XL4'00'                                                          
*                                                                               
ED#ADDFX EQU   ED#ADGAP+1          Default expiration period                    
         DC    AL2(ED#ADDFX),C'DefEx'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_DFTEX-SAVED)                                              
         DC    AL1(LD_SPAKQ,L'AD_DFTEX)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#ADDTV EQU   ED#ADDFX+1          Default no. of days to view estimate         
         DC    AL2(ED#ADDTV),C'DefNd'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_DFTND-SAVED)                                              
         DC    AL1(LD_SPAKQ,L'AD_DFTND)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADGEX EQU   ED#ADDTV+1          GAP estimate email extension                 
         DC    AL2(ED#ADGEX),C'GAPEX'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_GAPEX-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_GAPEX)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#ADGAR EQU   ED#ADGEX+1          GAP Approval required by all                 
         DC    AL2(ED#ADGAR),C'GAPAr'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_GAPAR-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_GAPAR)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#ADEML EQU   ED#ADGAR+1          Email address                                
         DC    AL2(ED#ADEML),C'Email'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_EMADD-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_EMADD)                                         
         DC    XL4'00'                                                          
*                                                                               
ED#ADBTY EQU   ED#ADEML+1          Billing type                                 
         DC    AL2(ED#ADBTY),C'BilTy'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_BILTY-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_BILTY)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADSGA EQU   ED#ADBTY+1          GAP Supplier setting                         
         DC    AL2(ED#ADSGA),C'SupGa'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_SGAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_SGAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADSAQ EQU   ED#ADSGA+1          GAP Supplier Acknowledge/Query               
         DC    AL2(ED#ADSAQ),C'AckQu'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_AQAP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_AQAP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#SWPDQ EQU   ED#ADSAQ+1          Suppress W/C printing default (est)          
         DC    AL2(ED#SWPDQ),C'Swcpd'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_SWPD-SAVED)                                               
         DC    AL1(LD_HEXDQ,L'AD_SWPD)                                          
         DC    XL4'00'                                                          
                                                                                
ED#DFAXQ EQU   ED#SWPDQ+1          Fax number                                   
         DC    AL2(ED#DFAXQ),C'FaxNo'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FAX-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_FAX)                                           
         DC    XL4'00'                                                          
                                                                                
ED#DTELQ EQU   ED#DFAXQ+1          Telephone number                             
         DC    AL2(ED#DTELQ),C'Telep'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_TELE-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_TELE)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADGED EQU   ED#DTELQ+1          GAP estimates email disclaimer               
         DC    AL2(ED#ADGED),C'GAPED'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_GAPED-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_GAPED)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADXAP EQU   ED#ADGED+1          Expenses approvers are chosen                
         DC    AL2(ED#ADXAP),C'ExApp'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_XAPP-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_XAPP)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADMIL EQU   ED#ADXAP+1          Expenses new mileage rates                   
         DC    AL2(ED#ADMIL),C'ExMil'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_XMIL-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_XMIL)                                          
         DC    XL4'00'                                                          
                                                                                
ED#AD1RO EQU   ED#ADMIL+1          1R office code                               
         DC    AL2(ED#AD1RO),C'1ROfc'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_1ROFF-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_1ROFF)                                         
         DC    XL4'00'                                                          
                                                                                
ED#AD1RD EQU   ED#AD1RO+1          1R department code                           
         DC    AL2(ED#AD1RD),C'1RDpt'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_1RDPT-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_1RDPT)                                         
         DC    XL4'00'                                                          
                                                                                
ED#AD1RS EQU   ED#AD1RD+1          1R Sub department code                       
         DC    AL2(ED#AD1RS),C'1RSub'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_1RSUB-SAVED)                                              
         DC    AL1(LD_CHARQ,L'AD_1RSUB)                                         
         DC    XL4'00'                                                          
                                                                                
ED#ADCCW EQU   ED#AD1RS+1           Colour coded work codes                     
         DC    AL2(ED#ADCCW),C'ADCCW'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_CCWC-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_CCWC)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADWCF EQU   ED#ADCCW+1           Colour coded work codes                     
         DC    AL2(ED#ADWCF),C'ADWCF'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_WCF-SAVED)                                                
         DC    AL1(LD_CHARQ,L'AD_WCF)                                           
         DC    XL4'00'                                                          
                                                                                
ED#ADPPO EQU   ED#ADWCF+1          Part approved orders resubmit                
         DC    AL2(ED#ADPPO),C'ADPPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_PPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_PPOR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADFPO EQU   ED#ADPPO+1          Fully approved orders resubmit               
         DC    AL2(ED#ADFPO),C'ADFPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_FPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_FPOR)                                          
         DC    XL4'00'                                                          
                                                                                
ED#ADMPO EQU   ED#ADFPO+1          Matched approved orders resubmit             
         DC    AL2(ED#ADMPO),C'ADMPO'                                           
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(AD_MPOR-SAVED)                                               
         DC    AL1(LD_CHARQ,L'AD_MPOR)                                          
         DC    XL4'00'                                                          
                                                                                
OUTACCDX DS    0X                                                               
                                                                                
**********************************************************************          
* COMMON SUB ROUTINES                                                *          
**********************************************************************          
**********************************************************************          
* check account is allowed to have time posted to it                 *          
**********************************************************************          
PROSPEC  NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*PROSPC*'                                                      
         CLC   ACTKULA-ACTRECD(2,R1),PRODUL                                     
         JNE   EXITY                                                            
         CLI   XL#APPL,RQ_ADTIM                                                 
         JNE   EXITY                                                            
         AHI   R1,ACTRFST-ACTRECD                                               
         USING RSTELD,R1                                                        
PROSP2   CLI   RSTEL,0                                                          
         JE    EXITY                                                            
         CLI   RSTEL,RSTELQ                                                     
         JE    PROSP4                                                           
                                                                                
         LLC   R0,RSTLN                                                         
         AR    R1,R0                                                            
         J     PROSP2                                                           
                                                                                
PROSP4   CLI   RSTLN,RSTLN3Q                                                    
         JL    EXITY                                                            
         TM    RSTSTAT5,RSTSNOTS   LOCKED FROM TEMPO TIMESHEETS                 
         JNZ   EXITN                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CUXPNUM        Check whether mobile                         
         CHI   RF,XPMOBILQ          app is connected                            
         JNE   EXITY                                                            
         TM    RSTLSTAT,RSTLSTIQ   Locked from t/s?                             
         JNZ   EXITN                                                            
         DROP  R1                                                               
                                                                                
PROSPY   J     EXITY                                                            
         EJECT                                                                  
**********************************************************************          
* Check job is on BrandOcean estimates                               *          
**********************************************************************          
ESTCHK   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*ESTCHK*'                                                      
         CLC   ACTKULA-ACTRECD(2,R1),PRODUL                                     
         JNE   EXITY                                                            
*&&UK                                                                           
         CLI   XL#APPL,RQ_ADEJL                                                 
         JNE   EXITY                                                            
*&&                                                                             
*&&US                                                                           
         CLI   XL#APPL,RQ_ADEJL                                                 
         JE    *+12                                                             
         CLI   XL#APPL,RQ_ADEST                                                 
         JNE   EXITY                                                            
*&&                                                                             
         AHI   R1,ACTRFST-ACTRECD                                               
         XR    R0,R0                                                            
                                                                                
         USING JOBELD,R1                                                        
ESTCHK2  CLI   JOBEL,0                                                          
         JE    EXITY                                                            
         CLI   JOBEL,JOBELQ                                                     
         JE    ESTCHK4                                                          
                                                                                
         IC    R0,JOBLN                                                         
         AR    R1,R0                                                            
         J     ESTCHK2                                                          
                                                                                
ESTCHK4  CLI   JOBLN,JOBLN3Q                                                    
         JL    EXITN                                                            
         TM    JOBSTA1,JOBSMCSE                                                 
         JZ    EXITN                                                            
         DROP  R1                                                               
                                                                                
ESTCHKY  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ACCOUNT SENDING ROUTINE                                             *         
***********************************************************************         
ACCSND   NTR1  LABEL=NO,WORK=(RC,ASWORKL)                                       
         J     *+12                                                             
         DC    C'*ACCSND*'                                                      
                                                                                
         USING ASWORKD,RC                                                       
         GOTOR CLRWRK,ASWORKL      Clear work area                              
         USING OB_D,ASRBAREA                                                    
         USING ACTRECD,R2                                                       
         LA    R0,AC_VALS                                                       
         LHI   R1,AC_LNQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   AC_NAM,SPACES                                                    
         MVC   AC_FNAM,SPACES                                                   
                                                                                
         CLC   RQ_ALLDG,=C'2P'     IF 2P MAKE SURE JUST OUTPUT ACCOUNT          
         JE    ACCS02                                                           
                                                                                
         CLI   RQ_ALSOR,YESQ                                                    
         JE    ACCS04                                                           
         CLI   XL#LIND,0                                                        
         JNE   ACCS14              list                                         
         CLC   =C'1R',ACTKULA      search                                       
         JE    ACCS12                                                           
ACCS02   MVC   AC_ULA,ACTKULA      - PASS ACCOUNT CODE                          
         J     ACCS18                                                           
ACCS04   CLI   LDGAL1,L'ACTKACT    - Department only special                    
         JE    ACCS06                only if office in 2D code                  
         MVC   TEMP2(1),LDGAOP                                                  
         NI    TEMP2,FF-LDGOKEY2                                                
         CLI   TEMP2,1                                                          
         JE    ACCS08                                                           
ACCS06   MVC   AC_ULA,ACTKACT                                                   
         J     ACCS18                                                           
                                                                                
ACCS08   XR    RE,RE                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         AHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   XL#COFF(0),ACTKACT                                               
         EX    RE,0(RF)                                                         
                                                                                
         LA    RE,ACTKACT+1                                                     
         LA    RF,L'ACTKACT-2                                                   
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    ACCS10                                                           
         AHI   RE,1                                                             
         SHI   RF,1                                                             
ACCS10   BASR  R1,0                                                             
         MVC   AC_ULA(0),0(RE)                                                  
         EX    RF,0(R1)                                                         
         J     ACCS18                                                           
                                                                                
ACCS12   LLC   RE,LDGAL3                                                        
         LR    R1,RE                                                            
         MVC   TEMP2,SPACES        - ONLY WANT PERSON CODE IF ALL               
         MVC   TEMP2(L'ACTKACT),ACTKACT                                         
         LA    R1,ACTKACT(R1)                                                   
         CLI   0(R1),C' '                                                       
         JNH   ACCS02                                                           
         J     ACCS16                                                           
ACCS14   MVC   TEMP2,SPACES        - OR CLIENT OR PRODUCT OR JOB ONLY           
         MVC   TEMP2(L'ACTKACT),ACTKACT                                         
         XR    RE,RE                                                            
         CLI   XL#LIND,1                                                        
         JE    ACCS16                                                           
         IC    RE,LDGAL1                                                        
         CLI   XL#LIND,2                                                        
         JE    ACCS16                                                           
         IC    RE,LDGAL2                                                        
         CLI   XL#LIND,3                                                        
         JE    ACCS16                                                           
         IC    RE,LDGAL3                                                        
         CLC   =C'1R',ACTKULA      Only want to call cost profiles              
         JNE   ACCS16               for 1R accounts and expenses                
         LLC   RE,ONERL1L                                                       
         SHI   RE,1                adjust for EX                                
         BASR  RF,0                                                             
         MVC   AC_1ROFF(0),ACTKACT                                              
         EX    RE,0(RF)                                                         
         AHI   RE,1                UNADJUST                                     
         LA    RF,ACTKACT                                                       
         AR    RF,RE                                                            
         LLC   R1,ONERL2L                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   AC_1RDPT(0),0(RF)                                                
         EX    R1,0(RE)                                                         
         AHI   R1,1                                                             
         AR    RF,R1                                                            
         LLC   RE,ONERL2L                                                       
         LLC   R1,ONERL3L                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         BASR  RE,0                                                             
         MVC   AC_1RSUB(0),0(RF)                                                
         EX    R1,0(RE)                                                         
         LLC   RE,LDGAL3                                                        
         CLI   RQ_ALAPL,RQ_ALEXP                                                
         JNE   ACCS16                                                           
         GOTOR (#CSTPRF,ACSTPRF),DMCB,XL#ACT                                    
         L     R3,ACOBLOCK                                                      
         USING COBLOCKD,R3                                                      
         MVC   AC_XAPP,COCAE                                                    
         MVC   AC_XMIL,COMIL                                                    
         DROP  R3                                                               
ACCS16   LA    RE,TEMP2(RE)                                                     
         MVC   AC_ULA,0(RE)                                                     
         MVC   OB_KEY(L'AC_ULA),AC_ULA                                          
         GOTOR GETBUF,OB_D         Do we already have this entry                
         JE    ACCS210             Yes                                          
ACCS18   CLC   ACTKULA(2),SGQ                                                   
         JNE   ACCS20                                                           
         MVC   AC_ULA,SPACES                                                    
         MVC   AC_ACT,ACTKACT                                                   
         USING NAMELD,R3                                                        
ACCS20   LA    R3,ACTRFST                                                       
         XC    BYTE3,BYTE3                                                      
         XC    AADREL,AADREL                                                    
         XC    AOATEL,AOATEL                                                    
         MVI   BYTE4,NOQ                                                        
*&&UK                                                                           
         CLI   RQ_ALRFN,YESQ                                                    
         JE    ACCS22                                                           
         TM    CPYSTATC,CPYSALTN+CPYSMEDN                                       
         JZ    ACCS34                                                           
ACCS22   CLC   =C'1R',ACTKULA      search                                       
         JE    ACCS34                                                           
         CLI   NAMEL,0                                                          
         JE    ACCS34                                                           
         TM    CPYSTATC,CPYSMEDN                                                
         JZ    ACCS24                                                           
         CLI   NAMEL,SNMELQ                                                     
         JE    ACCS28                                                           
ACCS24   CLI   NAMEL,XNMELQ                                                     
         JE    ACCS30                                                           
ACCS26   XR    R0,R0                                                            
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     ACCS22                                                           
*                                                                               
         USING SNMELD,R3                                                        
ACCS28   MVI   BYTE3,1                                                          
         LLC   R1,SNMLN                                                         
         SHI   R1,SNMLN1Q+1                                                     
         BASR  RF,0                                                             
         MVC   AC_NAM(0),SNMNAME   - PASS NAME                                  
         EX    R1,0(RF)                                                         
         J     ACCS26                                                           
*                                                                               
         USING XNMELD,R3                                                        
ACCS30   LLC   R1,XNMSUBL                                                       
         SHI   R1,1                                                             
         TM    CPYSTATC,CPYSALTN                                                
         JZ    ACCS32                                                           
         MVI   BYTE3,1                                                          
         BASR  RF,0                                                             
         MVC   AC_NAM(0),XNMSUBN   - PASS ALTERNATE NAME                        
         EX    R1,0(RF)                                                         
*                                                                               
ACCS32   CLI   RQ_ALRFN,YESQ                                                    
         JNE   ACCS26                                                           
         BASR  RF,0                                                             
         MVC   AC_FNAM(0),XNMSUBN  - PASS FOREIGN NAME                          
         EX    R1,0(RF)                                                         
         J     ACCS26                                                           
*                                                                               
         USING NAMELD,R3                                                        
ACCS34   LA    R3,ACTRFST                                                       
         MVI   XL#MYSW,NOQ                                                      
         MVI   AC_ANAD,NOQ                                                      
         MVI   AC_ANAP,NOQ                                                      
         MVI   AC_ANAM,NOQ                                                      
         MVI   AC_DLOCB,NOQ        SET LOCKED FROM BILLING=NO                   
         CLI   RQ_ALVIO,C' '                                                    
         JH    ACCS36                                                           
         MVI   XL#MYSW,YESQ                                                     
*&&                                                                             
ACCS36   CLI   NAMEL,0                                                          
         JE    ACCS84                                                           
         CLI   BYTE3,1                                                          
         JE    *+12                                                             
         CLI   NAMEL,NAMELQ                                                     
         JE    ACCS40                                                           
         CLI   NAMEL,RSTELQ                                                     
         JE    ACCS42                                                           
         CLI   NAMEL,EMPELQ                                                     
         JE    ACCS60                                                           
*&&UK*&& CLI   NAMEL,RATEVATQ                                                   
*&&UK*&& JE    ACCS54                                                           
         CLI   NAMEL,RATEDSCQ                                                   
         JE    ACCS56                                                           
         CLI   NAMEL,JOBELQ                                                     
         JE    ACCS62                                                           
         CLI   NAMEL,SPAELQ                                                     
         JE    ACCS68                                                           
         CLI   NAMEL,ADRELQ                                                     
         JE    ACCS70                                                           
         CLI   NAMEL,OATELQ                                                     
         JE    ACCS72                                                           
         CLI   NAMEL,ASTELQ                                                     
         JE    ACCS74                                                           
         CLI   NAMEL,DEXELQ                                                     
         JE    ACCS80                                                           
         CLI   NAMEL,FFTELQ                                                     
         JE    ACCS82                                                           
ACCS38   XR    R0,R0                                                            
         IC    R0,NAMLN                                                         
         AR    R3,R0                                                            
         J     ACCS36                                                           
*                                                                               
ACCS40   LLC   R1,NAMLN                                                         
         SHI   R1,3                                                             
         BASR  RF,0                                                             
         MVC   AC_NAM(0),NAMEREC   - PASS NAME                                  
         EX    R1,0(RF)                                                         
         J     ACCS38                                                           
*                                                                               
         USING RSTELD,R3                                                        
ACCS42   MVC   AC_CSTG,RSTCOSTG                                                 
         CLI   AC_CSTG,X'80'       unexplained unprintable chars in             
         JNE   *+8                    many cli/pro records                      
         MVI   AC_CSTG,C' '                                                     
         TM    RSTSTAT1,RSTSEADD                                                
         JZ    *+8                                                              
         MVI   AC_ANAD,YESQ                                                     
         TM    RSTSTAT1,RSTSGPEI                                                
         JZ    *+8                                                              
         MVI   AC_ANAP,YESQ                                                     
*&&UK                                                                           
         TM    RSTSTAT2,RSTSMILE                                                
         JZ    *+8                                                              
         MVI   AC_ANAM,YESQ                                                     
*&&                                                                             
         CLC   RQ_ALLDG,PRODUL                                                  
         JE    ACCS43                                                           
         MVI   AC_FPT,NOQ                                                       
         MVI   AC_FJT,NOQ                                                       
         MVI   AC_SGAP,NOQ                                                      
         MVI   AC_AQAP,NOQ                                                      
         TM    RSTSTAT4,RSTSJREA   TEST JOB REQ'D FOR EXP ANALYSIS              
         JZ    *+8                                                              
         MVI   AC_FJT,YESQ                                                      
                                                                                
         CLI   RSTLN,RSTLN3Q       CHECK RIGHT LENGTH ELEMENT                   
         JL    ACCS4A                                                           
         TM    RSTSTAT5,RSTSPREA   TEST PRODUCT REQUIRED                        
         JZ    *+8                                                              
         MVI   AC_FPT,YESQ                                                      
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   AC_SGAP,YESQ                                                     
         TM    RSTSTAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   AC_AQAP,YESQ                                                     
*&&                                                                             
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPQN   NO GAP SUPPLIER CONTROLS                     
         JZ    *+8                                                              
         MVI   AC_SGAP,NOQ                                                      
         TM    RSTSTAT7,RSTGAPAN   NO ACKNOWLEDGE/QUERY                         
         JZ    *+8                                                              
         MVI   AC_AQAP,NOQ                                                      
         TM    RSTSTAT7,RSTGAPQY   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   AC_SGAP,YESQ                                                     
         TM    RSTSTAT7,RSTGAPAY   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   AC_AQAP,YESQ                                                     
*&&                                                                             
*                                                                               
ACCS4A   GOTOR SUPGAP,DMCB,ACTKEY,AC_SGAP,AC_AQAP                               
*                                                                               
ACCS43   CLI   RSTLN,RSTLN3Q       CHECK ELEMENT RIGHT LENGTH                   
         JL    ACCS46                                                           
         CLI   RQ_ALAPL,RQ_ALORD   ARE WE IN ORDERS?                            
         JNE   ACCS44                                                           
         TM    RSTLSTAT,RSTLSORQ                                                
         JZ    ACCS50              NOT LOCKED                                   
         J     ACCS210             YES THEN SKIP                                
*                                  CHECK WHETEHR JOB LOCKED FROM EST            
ACCS44   CLI   RQ_ALAPL,RQ_ALEST   ARE WE IN ESTIMATES?                         
         JE    *+12                                                             
         CLI   RQ_ALAPL,RQ_ALEJL   ARE WE IN ESTIMATES JOB BILLING              
         JNE   ACCS46                                                           
         TM    RSTLSTAT,RSTLSESQ                                                
         JZ    ACCS50              NOT LOCKED                                   
         J     ACCS210             YES THEN SKIP                                
*                                                                               
ACCS46   CLI   RQ_ALAPL,RQ_ALTIM   ARE WE IN TIMESHEETS?                        
         JNE   ACCS48                                                           
         MVI   AC_FUTA,NOQ                                                      
         CLI   RSTLN,RSTLN3Q       CHECK ELEMENT RIGHT LENGTH                   
         JL    ACCS46A                                                          
         TM    RSTSTAT7,RSTSFUTM   FUTURE TIME ALLOWED?                         
         JZ    ACCS46A                                                          
         MVI   AC_FUTA,YESQ                                                     
         J     ACCS47                                                           
ACCS46A  CLI   RQ_ALFTM,YESQ       DO WE ONLY WANT FUTURE TIME ACCOUNTS         
         JE    ACCS210                                                          
         CLI   RSTLN,RSTLN3Q       CHECK ELEMENT RIGHT LENGTH                   
         JL    ACCS50                                                           
*                                                                               
ACCS47   TM    RSTLSTAT,RSTLSBIQ   JOB LOCKED FROM BILLING?                     
         JZ    *+12                                                             
         MVI   AC_DLOCB,YESQ       SET YES LOCKED FROM BILLING                  
         J     ACCS50                                                           
         TM    RSTLSTAT,RSTLSTIQ                                                
         JZ    ACCS50              NOT LOCKED                                   
         J     ACCS210             YES THEN SKIP                                
*                                                                               
ACCS48   CLI   RSTLN,RSTLN3Q       CHECK ELEMENT RIGHT LENGTH                   
         JL    ACCS50                                                           
         CLI   RQ_ALAPL,RQ_ALEXP   ARE WE IN EXPENSES?                          
         JNE   ACCS50                                                           
         TM    RSTLSTAT,RSTLSEXQ   LOCKED FROM EXTERNAL POSTINGS?               
         JZ    ACCS50                                                           
         J     ACCS210                                                          
*                                                                               
ACCS50   CLI   RQ_ALVIO,C' '                                                    
         JNH   ACCS38                                                           
         CLI   RQ_ALVIO,C'I'                                                    
         JNE   ACCS52                                                           
         TM    RSTSTAT1,RSTSIVAT                                                
         JZ    ACCS210             Skip account                                 
         J     ACCS38                                                           
                                                                                
ACCS52   TM    RSTSTAT1,RSTSIVAT                                                
         JNZ   ACCS210             skip account                                 
         J     ACCS38                                                           
*                                                                               
*&&UK                                                                           
         USING RATELD,R3                                                        
ACCS54   CLC   ACTKULA(2),SGQ      pass VAT rate on SG                          
         JNE   ACCS38                                                           
         ST    R2,FULL1                                                         
         XR    R2,R2                                                            
         ICM   R2,3,RATRATE                                                     
         CURED (R2),(L'AC_VAT,AC_VAT),0,ZERO=YES,ALIGN=LEFT                     
         L     R2,FULL1                                                         
         MVI   XL#MYSW,YESQ                                                     
         J     ACCS38                                                           
*&&                                                                             
*                                                                               
         USING RATELD,R3                                                        
ACCS56   CLC   ACTKULA(2),SVQ      pass discount rate on supplier               
         JE    ACCS58                                                           
         CLC   ACTKULA(2),SFQ                                                   
         JE    ACCS58                                                           
         CLC   ACTKULA(2),STQ                                                   
         JNE   ACCS38                                                           
ACCS58   ST    R2,FULL1                                                         
         XR    R2,R2                                                            
         ICM   R2,3,RATRATE                                                     
         CURED (R2),(L'AC_DISC,AC_DISC),0,ZERO=YES,ALIGN=LEFT                   
         L     R2,FULL1                                                         
         J     ACCS38                                                           
*                                                                               
         USING EMPELD,R3                                                        
ACCS60   CLC   LDGAUL,=C'1R'       THIS CHECK IS ON 1R ONLY                     
         JNE   ACCS38                                                           
         CLI   XL#APPL,RQ_ALEST    ESTIMATE MODULE                              
         JNE   ACCS38              NO                                           
         CLI   XL#ALL,YESQ         PERSON LIST                                  
         JNE   ACCS38              NO                                           
         CLI   EMPCSTAT,0          ACTIVE PERSON                                
         JE    ACCS38              YES - ALWAYS ALLOW                           
         J     ACCS210                                                          
*                                                                               
         USING JOBELD,R3                                                        
ACCS62   DS    0H                                                               
*&&US*&& TM    JOBSTA2,JOBSREJ                                                  
*&&US*&& JO    ACCS210                                                          
         CLI   XL#LIND,3          ONLY FOR JOB LEVEL                            
         JNE   ACCS38                                                           
         OC    RQ_ALMJO,RQ_ALMJO  ANYTHING PASSED?                              
         JZ    ACCS66                                                           
         CLI   RQ_ALMJO,YESQ      WANT ONLY MASTER JOBS?                        
         JNE   ACCS64                                                           
         TM    JOBSTA2,JOBSMST    THEN SKIP IF NOT MASTER                       
         JZ    ACCS210                                                          
         J     ACCS66                                                           
*                                                                               
ACCS64   TM    JOBSTA2,JOBSMST    NO MASTER THEN SKIP IF MASTER JOB             
         JNZ   ACCS210                                                          
         J     ACCS66                                                           
*                                                                               
ACCS66   MVI   AC_MJO,NOQ                                                       
         TM    JOBSTA2,JOBSMST                                                  
         JZ    *+8                                                              
         MVI   AC_MJO,YESQ        Set master job                                
         J     ACCS38                                                           
                                                                                
         USING SPAELD,R3                                                        
ACCS68   CLI   SPATYPE,SPATMJOB                                                 
         JNE   ACCS38                                                           
         MVC   AC_MJC,SPAAACT                                                   
         CLI   RQ_ALMJO,NOQ       No master or sub jobs                         
         JE    ACCS210            Is sub job so reject                          
         J     ACCS38                                                           
                                                                                
         USING ADRELD,R3                                                        
ACCS70   ST    R3,AADREL                                                        
         J     ACCS38                                                           
*                                                                               
         USING OATELD,R3                                                        
ACCS72   CLI   RQ_ALAPL,RQ_ALORD                                                
         JNE   ACCS38                                                           
         CLI   OATSUB,OATSUB5Q                                                  
         JNE   ACCS38                                                           
         ST    R3,AOATEL                                                        
         J     ACCS38                                                           
*                                                                               
         USING ASTELD,R3                                                        
ACCS74   CLC   AGYCURR,ASTCUR                                                   
         JE    ACCS76                                                           
         MVC   AC_CUR,ASTCUR                                                    
         OC    AC_CUR,SPACES                                                    
         CLI   ASTCUR,ASTCANY                                                   
         JNE   ACCS76                                                           
         MVC   AC_CUR,=CL3'***'                                                 
ACCS76   MVI   AC_13BVI,NOQ                                                     
         MVI   AC_ISFO,NOQ                                                      
         TM    ASTSTAT1,ASTISFOR   Is foreign language user                     
         JZ    *+8                                                              
         MVI   AC_ISFO,YESQ                                                     
         CLI   RQ_ALAPL,RQ_ALINV   TEST INVOICES                                
         JNE   ACCS38                                                           
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         JNE   ACCS38                                                           
*&&UK                                                                           
         CLI   AST13VAT,C' '       TEST VAT CODE SET                            
         JNH   *+14                                                             
         MVC   AC_VATC(1),AST13VAT                                              
*&&                                                                             
         MVI   AC_13BVI,YESQ                                                    
         CLI   ASTKSVTY,0          TEST KSV TYPE SET                            
         JNH   ACCS38                                                           
         MVC   FULL1,XL#TODP                                                    
         CLC   RQ_ALIDT,SPACES     TEST INVOICE DATE PASSED                     
         JNH   ACCS77                                                           
         GOTOR VDATCON,DMCB,(0,RQ_ALIDT+2),(1,FULL1)                            
ACCS77   LA    RF,WORK             GET RATE FOR KSV TYPE                        
         USING CONBLKD,RF                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONFLD,CONFKSV      KSV CALL                                     
         MVI   CONACTN,CONAGETQ    GET KSV DETAILS                              
         MVI   CONILEN,L'ASTKSVTY  LENGTH OF CODE                               
         LA    RE,ASTKSVTY                                                      
         STCM  RE,15,CONIADD       INPUT ADDRESS                                
         LA    RE,TEMP                                                          
         STCM  RE,15,CONOADD       OUTPUT ADDRESS                               
         MVC   CONCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   CONKSVDT,FULL1      EFFECTIVE KSV DATE                           
         GOTO1 VCONVERT,CONBLK                                                  
         JE    *+14                                                             
ACCS78   MVC   LP_ERROR,=AL2(AE$NOKSV)                                          
         J     XERROR                                                           
         DROP  RF                                                               
         ZAP   DUB1,TEMP(3)                                                     
         CURED (P8,DUB1),(L'AC_KSVR,AC_KSVR),0,ZERO=YES,ALIGN=LEFT              
         J     ACCS38                                                           
*                                                                               
         USING DEXELD,R3                                                        
ACCS80   CLI   RQ_ALAPL,RQ_ALINV   TEST INVOICES                                
         JNE   ACCS38                                                           
         GOTOR GETDDF,DMCB,DEXELD,AC_DDXP  RETURN DUE DATE FORMULA              
         J     ACCS38                                                           
                                                                                
         USING FFTELD,R3                                                        
ACCS82   DS    0H                                                               
*&&UK                                                                           
         CLI   FFTTYPE,FFTTVATC    VAT CODE                                     
         JE    *+12                                                             
         CLI   FFTTYPE,FFTTG13B    13B VAT CODE                                 
         JNE   ACCS83                                                           
         CLI   RQ_ALAPL,RQ_ALINV   TEST INVOICES                                
         JNE   ACCS38                                                           
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         JNE   ACCS38                                                           
         LA    RF,AD_VATC                                                       
         CLI   AD_VATC,C' '        MAY BE A 13B CODE FROM ASTEL ALREADY         
         JNH   *+8                                                              
         AHI   RF,1                SO APPEND OTHERS                             
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,RF),FFTDATA     UP TO FOUR MORE                              
         EX    RE,0(R1)                                                         
         J     ACCS38                                                           
*&&                                                                             
                                                                                
ACCS83   DS    0H                                                               
*&&US                                                                           
ACCS83A  CLI   FFTTYPE,FFTTBEML    USE ORDER AUTHORISER IF NOT                  
         JNE   ACCS83B             FOUND USE EMAIL ON 1ST PAGE                  
         MVI   BYTE4,YESQ                                                       
         J     ACCS83C                                                          
*&&                                                                             
ACCS83B  CLI   BYTE4,YESQ          HAVE WE FOUND ORDER AUTHORISER               
         JE    ACCS83D             YES                                          
*&&UK*&& CLI   FFTTYPE,FFTTPEML    EMAIL?                                       
*&&US*&& CLI   FFTTYPE,FFTTEML                                                  
         JNE   ACCS83D                                                          
ACCS83C  MVC   AC_EMADD,SPACES                                                  
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   AC_EMADD(0),FFTDATA                                              
         EX    RF,0(R1)                                                         
         J     ACCS38                                                           
                                                                                
ACCS83D  CLI   FFTTYPE,FFTTPFAX    FAX?                                         
         JNE   ACCS83E                                                          
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   AC_FAX(0),FFTDATA                                                
         EX    RF,0(R1)                                                         
         J     ACCS38                                                           
                                                                                
ACCS83E  CLI   FFTTYPE,FFTTPTEL    TELEPHONE?                                   
         JNE   ACCS38                                                           
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   AC_TELE(0),FFTDATA                                               
         EX    RF,0(R1)                                                         
         J     ACCS38                                                           
                                                                                
ACCS84   L     R3,AADREL                                                        
         OC    AOATEL,AOATEL                                                    
         JZ    ACCS88                                                           
         L     R3,AOATEL                                                        
         USING OATELD,R3                                                        
*&&UK                                                                           
         XR    R1,R1                                                            
         ICM   R1,1,OATNUM         N'ADDRESS LINES                              
         JZ    ACCS92                                                           
         CHI   R1,4                                                             
         JNH   *+8                                                              
         LA    R1,4                                                             
         LA    RF,OATADD1                                                       
         LA    RE,AC_ADDR1                                                      
         J     ACCS90                                                           
*&&                                                                             
*&&US                                                                           
         LA    R1,2                                                             
         LA    RF,OATLINE1                                                      
         LA    RE,AC_ADDR1                                                      
         TM    OATSTAT,OATCSZ                                                   
         JZ    ACCS86                                                           
         MVC   AC_ADDR3(L'OATLINE1),OATCSZZR                                    
                                                                                
ACCS86   MVC   0(L'OATLINE1,RE),0(RF)                                           
         AHI   RE,L'OATLINE1                                                    
         AHI   RF,L'OATLINE1                                                    
         JCT   R1,ACCS86                                                        
         J     ACCS94                                                           
*&&                                                                             
         USING ADRELD,R3                                                        
ACCS88   XR    R1,R1                                                            
         ICM   R1,1,ADRNUM         N'ADDRESS LINES                              
         JZ    ACCS92                                                           
         CHI   R1,4                                                             
         JNH   *+8                                                              
         LA    R1,4                                                             
         LA    RF,ADRADD1                                                       
         LA    RE,AC_ADDR1                                                      
                                                                                
ACCS90   MVC   0(L'ADRADD1,RE),0(RF)                                            
         AHI   RE,L'OATLINE1                                                    
         AHI   RF,L'ADRADD1                                                     
         JCT   R1,ACCS90                                                        
                                                                                
ACCS92   DS    0H                                                               
*&&UK                                                                           
         CLI   XL#MYSW,YESQ        Vat rate found?                              
         JNE   ACCS210                                                          
*&&                                                                             
         TM    ACTRSTAT,ACTSLOCK                                                
         JZ    ACCS94                                                           
         MVI   AC_LOC,YESQ                                                      
*                                                                               
ACCS94   TM    ACTRSTAT,ACTSCLOS                                                
         JZ    ACCS96                                                           
         MVI   AC_CLO,YESQ                                                      
                                                                                
ACCS96   MVC   SAVEKEY,IOKEY                                                    
         GOTOR GETAGN,DMCB,ACTKULA,AC_AGACC,AC_AGAD1                            
         MVC   AC_OFFC,XL#COFF                                                  
         CLC   XL#COFF,SPACES                                                   
         JH    *+10                                                             
         MVC   AC_OFFC,XL#HOFF                                                  
         OC    AC_OFFC,SPACES                                                   
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(2),=C'2D'                                                  
         MVC   TEMP2+2(L'AC_OFFC),AC_OFFC                                       
         GOTOR (#GETACN,AGETACN)         GET OFFICE NAME                        
         MVC   IOKEY,SAVEKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   AC_OFFN,TEMP2                                                    
*                                                                               
ACCS98   TM    ACTRSTAT,ACTSDRFT                                                
         JZ    ACCS100                                                          
         MVI   AC_DRA,YESQ                                                      
*                                                                               
ACCS100  CLC   ACTKULA(2),PRODUL                                                
         JNE   ACCS200                                                          
         MVC   SJACLIC,SPACES                                                   
         MVC   SJAPROC,SPACES                                                   
         MVC   SJAJOBC,SPACES                                                   
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   SJACLIC(0),ACTKACT                                               
         EX    RE,0(R1)                                                         
         LA    RE,ACTKACT+1(RE)                                                 
         LLC   R1,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   SJAPROC(0),0(RE)                                                 
         EX    RF,0(R1)                                                         
         AR    RE,RF                                                            
         AHI   RE,1                                                             
         LLC   RF,PPROLEN                                                       
         LLC   R1,PJOBLEN                                                       
         SR    R1,RF                                                            
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   SJAJOBC(0),0(RE)                                                 
         EX    R1,0(RF)                                                         
*                                                                               
         USING GOBLOCK,R3                                                       
         L     R3,AGOBLOCB                                                      
*                                                                               
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
*                                                                               
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US USES 1ST EXTENSION BLOCK                  
         MVC   GOABEXT,AGOBBLCK    UK USES 2ND EXTENSION BLOCK                  
         MVC   GOABUFF,AGENAREA                                                 
         LHI   RE,GENAREAX-GENAREA                                              
         ST    RE,GOLBUFF                                                       
*                                                                               
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELCLI(L'SJACLIC),SJACLIC                                      
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO(L'SJAPROC),SJAPROC                                      
         MVC   GOSELJOB,SJAJOBC                                                 
         MVC   GOCTRY,CUCTRY                                                    
         CLI   RQ_ALMED,C' '                                                    
         JNH   *+10                                                             
         MVC   GOSELMED,RQ_ALMED                                                
         CLI   RQ_ALWC,C' '                                                     
         JNH   *+14                                                             
         MVC   GOSELWC,RQ_ALWC                                                  
         MVI   GOANYWC,YESQ                                                     
         L     RF,ACOMFACS                                                      
                                                                                
         XC    GOACOVL,GOACOVL                                                  
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    *+10                                                             
         MVC   GOACOVL,VCOVAIL                                                  
         MVC   GOABINSR,CBINSRCH-COMFACSD(RF)                                   
                                                                                
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
         MVC   AC_NAE,GONEEDAE                                                  
*&&US*&& MVI   AD_ISFO,C'N'                                                     
*&&UK                                                                           
         CLI   XL#LIND,3                                                        
         JNL   *+10                                                             
         MVC   AC_ISFO,GOJULDEF                                                 
         MVC   AC_BKSV,GOBILKSV                                                 
*&&                                                                             
         MVC   AC_BILTY,GOBILTYP                                                
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
         MVC   AC_GAP,GOGAPS                                                    
         MVC   AC_SWPD,GOSWPD                                                   
         LLC   RF,GOGDES                                                        
         CVD   RF,DUB                                                           
         MVC   AC_DFTEX,DUB+6                                                   
         LLC   RF,GODNDV                                                        
         CVD   RF,DUB                                                           
         MVC   AC_DFTND,DUB+6                                                   
         MVC   AC_GAPAR,GOGARA                                                  
         MVC   AC_GAPEX,GOGEMX                                                  
         MVC   AC_GAPED,GOGEMD                                                  
*                                                                               
ACCS102  CLI   RQ_ALAPL,RQ_ALTIM      ONLY FOR TIMESHEETS                       
         JNE   ACCS116                                                          
         CLI   RQ_ALFPT,YESQ          PER2 OVERRIDE FOR SHOWING PRODUCT         
         JNE   ACCS103                                                          
         MVI   AC_FPT,C'A'            FORCE PRODUCT                             
         J     ACCS104                                                          
*                                                                               
ACCS103  MVC   AC_FPT,GOFPT                                                     
*                                                                               
ACCS104  CLI   RQ_ALFJT,YESQ          PER2 OVERRIDE FOR SHOWING JOB             
         JNE   ACCS105                                                          
         MVI   AC_FJT,C'A'            FORCE JOB                                 
         J     ACCS106                                                          
*                                                                               
ACCS105  MVC   AC_FJT,GOFJT                                                     
*                                                                               
ACCS106  MVC   AC_DEFT,GOTOT                                                    
                                                                                
*&&UK                                                                           
         USING GOBLOCK,R3                                                       
         L     R3,AGOBLOCB                                                      
*&&                                                                             
*&&US                                                                           
         USING GOXBLKD,R3                                                       
         L     R3,AGOXBLCK                                                      
*&&                                                                             
         LA    RF,GOTTALLW                                                      
         LA    R1,3                                                             
         MVC   AC_BILA(L'AC_BILA+L'AC_CHGA+L'AC_NBLA),=C'NNN'                   
ACCS110  CLI   0(RF),C'B'                                                       
         JNE   *+8                                                              
         MVI   AC_BILA,C'Y'                                                     
         CLI   0(RF),C'R'                                                       
         JNE   *+8                                                              
         MVI   AC_CHGA,C'Y'                                                     
         CLI   0(RF),C'N'                                                       
         JNE   *+8                                                              
         MVI   AC_NBLA,C'Y'                                                     
         LA    RF,1(RF)                                                         
         JCT   R1,ACCS110                                                       
                                                                                
         CLI   RQ_ALAPL,RQ_ALTIM      CHECK TIME APPLICATION                    
         JNE   ACCS112                                                          
         CLI   RQ_ALFJT,YESQ       JOB INPUT REQUIRED OVERRIDES                 
         JE    *+10                                                             
         MVC   AC_JOBT,GOTNOJOB                                                 
                                                                                
ACCS112  LA    R3,GOTFNARR                                                      
         LA    R1,3                                                             
         MVC   AC_FNTB(L'AC_FNTB+L'AC_FNTR+L'AC_FNTN),=C'NNN'                   
ACCS114  CLI   0(R3),C'B'                                                       
         JNE   *+8                                                              
         MVI   AC_FNTB,C'Y'                                                     
         CLI   0(R3),C'R'                                                       
         JNE   *+8                                                              
         MVI   AC_FNTR,C'Y'                                                     
         CLI   0(R3),C'N'                                                       
         JNE   *+8                                                              
         MVI   AC_FNTN,C'Y'                                                     
         LA    R3,1(R3)                                                         
         JCT   R1,ACCS114                                                       
*                                                                               
         USING GOXBLKD,R3                                                       
ACCS116  L     R3,AGOXBLCK                                                      
         MVC   AC_NJLE,GONJLE                                                   
         MVC   AC_BILO,GOBILO                                                   
         MVC   AC_CCWC,GOCCW      COLOUR CODED WORK CODES                       
         MVC   AC_FPOR,GOFPORES   Fully approved order resubmit rules           
         MVC   AC_PPOR,GOPPORES   Part approved order resubmit rules            
         MVC   AC_MPOR,GOMPORES   Matched order resubmit rules                  
         MVC   AC_WCF,GOWCF       USE WC FLAG FOR ESTIMATE CHECK                
         MVI   AC_MDOWC,C'M'                                                    
*&&UK                                                                           
         CLI   GOICRA,GOIWCSI                                                   
         JL    *+8                                                              
*&&                                                                             
         MVI   AC_MDOWC,C'W'                                                    
         MVC   SAVEKEY,IOKEY                                                    
         GOTOR GETMED,DMCB,ACTKULA,AC_MINC                                      
         MVC   IOKEY,SAVEKEY                                                    
*&&UK                                                                           
         CLI   GOICRA,GOIMESK                                                   
         JNE   ACCS142                                                          
*&&                                                                             
         MVC   AC_MINC(L'ACTKUNT+L'ACTKLDG),=C'SK'                              
                                                                                
ACCS142  CLI   RQ_ALAPL,RQ_ALTIM   CHECK TIME APPLICATION                       
         JNE   ACCS144                                                          
         CLI   XL#LIND,3           CHECK JOB LEVEL REQUEST                      
         JNE   ACCS144                                                          
         CLI   AC_JOBT,NOQ         JOB INPUT ALLOWED ON TIME?                   
         JE    ACCS210             NO                                           
                                                                                
ACCS144  CLI   RQ_ALAPL,RQ_ALORD   ORDERS                                       
         JE    ACCS146                                                          
         CLI   RQ_ALAPL,RQ_ALEXP   EXPENSES                                     
         JE    ACCS146                                                          
         CLI   RQ_ALAPL,RQ_ALINV   INVOICES                                     
         JNE   ACCS148                                                          
ACCS146  CLI   RQ_ALBIL,NOQ        IS THE ENTRY BILLABLE WITHIN MODULE          
         JNE   ACCS148             YES                                          
         CLI   AC_BILO,YESQ        NO - IS IT BILLABLE ONLY                     
         JE    ACCS210             YES - SKIP THIS ACCOUNT                      
                                                                                
ACCS148  CLI   RQ_ALAPL,RQ_ALEST  Estimates                                     
         JE    *+12                                                             
         CLI   RQ_ALAPL,RQ_ALEJL  Estimates                                     
         JNE   ACCS150                                                          
         CLI   AC_DRA,YESQ                                                      
         JNE   ACCS150                                                          
         CLI   GOAEDT,NOQ                                                       
         JE    ACCS210                                                          
*                                                                               
         USING GOBBLKD,RF                                                       
ACCS150  L     RF,AGOBBLCK                                                      
         CLC   AC_CUR,SPACES                                                    
         JH    ACCS200                                                          
         CLC   GOBILCUR,AGYCURR                                                 
         JE    ACCS200                                                          
         MVC   AC_CUR,GOBILCUR                                                  
         DROP  RF                                                               
*                                                                               
ACCS200  CLC   =C'1R',ACTKULA      Is it ledger 1R                              
         JNE   ACCS205             No                                           
         MVC   OB_KEY(L'AC_ULA),AC_ULA Yes - add to buffer to ensure            
         GOTOR ADDBUF,OB_D         person code is note duplicated               
                                                                                
ACCS205  GOTOR LP_APUTO,LP_D                                                    
         LLC   RF,XL#COUNT                                                      
         AHI   RF,1                                                             
         STC   RF,XL#COUNT                                                      
*                                                                               
ACCS210  J     EXITY                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
ASWORKD  DSECT                     ** ACCSND local w/s **                       
ASRBAREA DS    XL(OB_LNQ)                                                       
         ORG   ASRBAREA+(OB_OTHER-OB_D)                                         
         ORG                                                                    
ASWORKL  EQU   *-ASWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Read for production media record                                    *         
***********************************************************************         
                                                                                
GETMED   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*GETMED*'                                                      
                                                                                
         CLI   XL#APPL,RQ_ADORD    IS IS ORDERS MODULE                          
         JNE   EXITY               NO - NOT INTERESTED                          
         LM    R3,R4,0(R1)         R3=INPUT ACCOUNT,R4=OUTPUT INCOME AC         
         USING ACTKULA,R3                                                       
INC      USING ACTKULA,R4                                                       
         CLC   ACTKULA(L'PRODUL),PRODUL  IS IT PRODUCTION LEDGER                
         JNE   EXITY               NO - NOT INTERESTED                          
                                                                                
         LA    RF,ACTKACT                                                       
         LLC   RE,PPROLEN                                                       
         AR    RF,RE                                                            
         LLC   R1,PJOBLEN                                                       
         SR    R1,RE                                                            
         SHI   R1,1                                                             
         JM    EXITY                                                            
         BASR  RE,0                                                             
         CLC   0(0,RF),SPACES      HAVE WE GOT A JOB                            
         EX    R1,0(RE)                                                         
         JNH   EXITY               NO                                           
                                                                                
         USING PMDRECD,R2                                                       
         LA    R2,IOKEY            READ 1ST CHAR FOR MEDIA CODE                 
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKCPY,CUXCPY                                                   
         MVC   PMDKMED,0(RF)                                                    
         CLC   PMDKMED,XL#SVMED    HAVE WE READ MEDIA BEFORE                    
         JE    GETMED10            YES                                          
         MVC   XL#SVMED,PMDKMED    NO - SAVE NEW MEDIA                          
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIO3                                                          
         LA    R2,PMDRFST                                                       
         USING PMDELD,R2                                                        
                                                                                
GETMED02 CLI   PMDEL,PMDELQ                                                     
         JE    GETMED04                                                         
         CLI   PMDEL,0                                                          
         JE    GETMED08                                                         
         LLC   R0,PMDLN                                                         
         AR    R2,R0                                                            
         J     GETMED02                                                         
                                                                                
GETMED04 CLI   PMDLN,PMDLN2Q                                                    
         JL    GETMED06                                                         
         MVC   INC.ACTKULA,PMDCOM2+1   Set account                              
                                                                                
GETMED06 CLC   INC.ACTKULA,SPACES                                               
         JH    EXITY                                                            
         MVC   INC.ACTKULA,PMDCOMU1                                             
                                                                                
GETMED08 MVC   XL#SVINC,INC.ACTKULA                                             
         CLC   INC.ACTKULA,SPACES                                               
         JH    EXITY                                                            
         MVC   ROUERRV,=AL2(AE$MIACC)                                           
         J     EXITN                                                            
                                                                                
GETMED10 MVC   INC.ACTKULA,XL#SVINC                                             
         DROP  R2,R3,INC                                                        
                                                                                
GETMEDY  J     EXITY                                                            
                                                                                
***********************************************************************         
* Get agent details for ST account                                    *         
***********************************************************************         
                                                                                
GETAGN   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*GETAGN*'                                                      
                                                                                
         CLI   XL#APPL,RQ_ADORD    IS IS ORDERS MODULE                          
         JNE   GETAGNY             NO - NOT INTERESTED                          
         LM    R3,R5,0(R1)         R3=ST ACCOUNT,R4=AGENT CODE,R5=ADRS          
         USING ACTKULA,R3                                                       
                                                                                
         CLC   =C'ST',ACTKULA      FOR ST GET AGENT DETAILS                     
         JNE   EXITY                                                            
         XR    RE,RE                                                            
         IC    RE,LDGAL3                                                        
         CLI   LDGAL4,L'ACTKACT                                                 
         JE    GETAGN02                                                         
         IC    RE,LDGAL2                                                        
         CLI   LDGAL3,L'ACTKACT                                                 
         JE    GETAGN02                                                         
         IC    RE,LDGAL1                                                        
         CLI   LDGAL2,L'ACTKACT                                                 
         JE    GETAGN02                                                         
         DC    H'0'                WHERE IS THE AGENT THEN?                     
*                                                                               
GETAGN02 AHI   RE,1                                                             
         MVC   TEMP2(14),SPACES                                                 
         BASR  R1,0                                                             
         MVC   TEMP2(0),ACTKULA                                                 
         EX    RE,0(R1)                                                         
         MVC   0(L'ACTKULA,R4),TEMP2                                            
         CLC   XL#AGACC,TEMP2      IS IT SAME AS LAST TIME READ                 
         JE    GETAGN12            YES - GET DETAILS SAVED                      
         MVC   XL#AGACC,TEMP2      NO                                           
         GOTOR (#GETACN,AGETACN)   GET ACCOUNT NAME                             
         MVC   L'ACTKULA(L'NAMEREC,R4),TEMP2                                    
         MVC   XL#AGNAM,TEMP2                                                   
         MVC   XL#AGAD1(4*L'XL#AGAD1),SPACES                                    
         L     R2,AIO3                                                          
         AHI   R2,ACTRFST-ACTRECD                                               
         USING ADRELD,R2                                                        
GETAGN04 CLI   ADREL,0                                                          
         JE    EXITY                                                            
         CLI   ADREL,ADRELQ                                                     
         JE    GETAGN08                                                         
         LLC   R0,ADRLN                                                         
         AR    R2,R0                                                            
         J     GETAGN04                                                         
                                                                                
GETAGN08 XR    R1,R1                                                            
         ICM   R1,1,ADRNUM         N'ADDRESS LINES                              
         JZ    EXITY                                                            
         CHI   R1,4                                                             
         JNH   *+8                                                              
         LA    R1,4                                                             
         LA    RF,ADRADD1                                                       
         LR    RE,R5                                                            
         LA    R3,XL#AGAD1                                                      
                                                                                
GETAGN10 MVC   0(L'ADRADD1,RE),0(RF)                                            
         MVC   0(L'ADRADD1,R3),0(RF)                                            
         AHI   RE,L'ADRADD1                                                     
         AHI   R3,L'ADRADD1                                                     
         AHI   RF,L'ADRADD1                                                     
         JCT   R1,GETAGN10                                                      
         J     EXITY                                                            
                                                                                
GETAGN12 MVC   0(4*L'ADRADD1,R5),XL#AGAD1                                       
         MVC   L'ACTKULA(L'NAMEREC,R4),XL#AGNAM                                 
         DROP  R2,R3                                                            
                                                                                
GETAGNY  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* Read down supplier levels to get settings                           *         
* ON NTRY P1=A(supplier account)                                      *         
*         P2,P3=A(GAP/ACKNOWLEDGE SETTINGS)                           *         
***********************************************************************         
         USING ACTRECD,R3                                                       
         USING OB_D,X#OBAREA                                                    
SUPGAP   NTR1  LABEL=NO,WORK=(RC,VGWORKL)                                       
         J     *+12                                                             
         DC    C'*SUPGAP*'                                                      
                                                                                
         LM    R3,R5,0(R1)                                                      
                                                                                
         USING VGWORKD,RC                                                       
         GOTOR CLRWRK,VGWORKL      Clear work area                              
         USING OB_D,VGRBAREA                                                    
                                                                                
*&&US                                                                           
         CLI   0(R4),YESQ          Is the GAP set at the lowest level           
         JNE   *+12                No - read higher levels                      
         CLI   0(R5),YESQ                                                       
         JE    EXITY               Yes - nothing to do                          
*&&                                                                             
*&&UK                                                                           
         CLI   0(R4),C' '                                                       
         JNH   *+12                                                             
         CLI   0(R5),C' '                                                       
         JH    EXITY                                                            
*&&                                                                             
*                                                                               
         CLC   =C'SX',ACTKULA      Only needed for suppliers                    
         JE    SUPGAP01                                                         
         CLC   =C'SV',ACTKULA                                                   
         JE    SUPGAP01                                                         
         CLC   =C'ST',ACTKULA                                                   
         JE    SUPGAP01                                                         
         CLC   =C'SF',ACTKULA                                                   
         JNE   EXITY                                                            
                                                                                
SUPGAP01 CLI   LDGAL1,L'ACTKACT                                                 
         JE    EXITY                                                            
         LA    R0,4                                                             
         LA    R6,LDGAL4                                                        
SUPGAP02 CLI   0(R6),0                                                          
         JE    SUPGAP04                                                         
         CLI   0(R6),L'ACTKACT                                                  
         JE    SUPGAP04                                                         
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         LA    RF,ACTKACT(RF)      FIND LENGTH OF ACCOUNT                       
         CLI   0(RF),C' '                                                       
         JH    SUPGAP06                                                         
SUPGAP04 SHI   R6,1                                                             
         JCT   R0,SUPGAP02                                                      
         J     EXITY                                                            
*                                                                               
K        USING ACTRECD,IOKEY                                                    
SUPGAP06 ST    R0,FULL2                                                         
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCPY,CUXCPY                                                 
         MVC   K.ACTKUNT(L'ACTKUNT+L'ACTKLDG),ACTKUNT                           
         LLC   RF,0(R6)                                                         
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   K.ACTKACT(0),ACTKACT                                             
         EX    RF,0(RE)                                                         
         MVC   OB_KEY(L'ACTKEY),K.ACTKEY                                        
         GOTOR GETBUF,OB_D                                                      
         JL    SUPGAP08                                                         
         JH    EXITN                                                            
*&&US                                                                           
         TM    OB_STAT7,RSTGAPYN   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         TM    OB_STAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   0(R5),YESQ                                                       
*&&                                                                             
*&&UK                                                                           
         CLI   0(R4),C' '                                                       
         JH    SUPGAP07                                                         
         TM    OB_STAT7,RSTGAPQN   NO GAP SUPPLIER CONTROLS                     
         JNZ   *+8                                                              
         MVI   0(R4),NOQ                                                        
         TM    OB_STAT7,RSTGAPQY   GAP SUPPLIER CONTROLS                        
         JNZ   *+8                                                              
         MVI   0(R4),YESQ                                                       
*                                                                               
SUPGAP07 CLI   0(R5),C' '                                                       
         JH    SUPGAP10                                                         
         TM    OB_STAT7,RSTGAPAN   NO ACKNOWLEDGE/QUERY                         
         JNZ   *+8                                                              
         MVI   0(R5),NOQ                                                        
         TM    OB_STAT7,RSTGAPAY   ACKNOWLEDGE/QUERY                            
         JNZ   *+8                                                              
         MVI   0(R5),YESQ                                                       
*&&                                                                             
         J     SUPGAP10                                                         
                                                                                
SUPGAP08 GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR GETELA,RSTELQ       Locate record status element                 
         JE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R1                                                        
         MVC   OB_STAT7,RSTSTAT7                                                
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   0(R4),YESQ                                                       
         TM    RSTSTAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   0(R5),YESQ                                                       
*&&                                                                             
*&&UK                                                                           
         CLI   0(R4),C' '                                                       
         JH    SUPGAP09                                                         
         TM    OB_STAT7,RSTGAPQN   NO GAP SUPPLIER CONTROLS                     
         JNZ   *+8                                                              
         MVI   0(R4),NOQ                                                        
         TM    OB_STAT7,RSTGAPQY   GAP SUPPLIER CONTROLS                        
         JNZ   *+8                                                              
         MVI   0(R4),YESQ                                                       
*                                                                               
SUPGAP09 CLI   0(R5),C' '                                                       
         JH    SUPGAP9A                                                         
         TM    OB_STAT7,RSTGAPAN   NO ACKNOWLEDGE/QUERY                         
         JNZ   *+8                                                              
         MVI   0(R5),NOQ                                                        
         TM    OB_STAT7,RSTGAPAY   ACKNOWLEDGE/QUERY ALL                        
         JNZ   *+8                                                              
         MVI   0(R5),YESQ                                                       
*                                                                               
SUPGAP9A DS    0H                                                               
*&&                                                                             
         GOTOR ADDBUF,OB_D         SAVE TO BUFFER                               
*                                                                               
SUPGAP10 DS    0H                                                               
         L     R0,FULL2            reset R0                                     
*&&US                                                                           
         CLI   0(R4),YESQ          BOTH SETTINGS SET TO YES YET?                
         JNE   *+12                                                             
         CLI   0(R5),YESQ                                                       
         JE    EXITY                                                            
*&&                                                                             
*&&UK                                                                           
         CLI   0(R4),C' '          BOTH SETTINGS SET YET?                       
         JNH   *+12                                                             
         CLI   0(R5),C' '                                                       
         JH    EXITY                                                            
*&&                                                                             
         J     SUPGAP04                                                         
*                                                                               
         DROP  K,R1,R3                                                          
VGWORKD  DSECT                     ** SUPGAP local w/s **                       
VGRBAREA DS    XL(OB_LNQ)                                                       
         ORG   VGRBAREA+(OB_OTHER-OB_D)                                         
OB_STAT7 DS    CL(L'RSTSTAT7)                                                   
         ORG                                                                    
VGWORKL  EQU   *-VGWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* Clear work area (RC=A(Work area), R1=L'Work area)                   *         
***********************************************************************         
                                                                                
CLRWRK   STM   RE,R1,12(RD)                                                     
         LR    R0,RC                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LM    RE,R1,12(RD)                                                     
         BR    RE                                                               
                                                                                
                                                                                
***********************************************************************         
* Locate an element in record pointed to by IOADDR                    *         
*                                                                     *         
* Ntry:- R1=Element code                                              *         
* Exit:- R1=A(Element) and CC=Equal if element found                  *         
*        R1=0 and CC=Not equal if element not found                   *         
***********************************************************************         
                                                                                
GETELA   LR    RF,R1               RF=Element code to search for                
         L     R1,IOADDR                                                        
         AHI   R1,ACCRFST-ACCRECD                                               
         SR    R0,R0                                                            
GETELA02 CLI   0(R1),0             Test end of record                           
         JNE   *+10                                                             
         SR    R1,R1               Yes - clear pointer address                  
         CR    RE,R1               Set CC to not equal                          
         BR    RE                                                               
         CLM   RF,1,0(R1)          Test correct element                         
         BER   RE                  Yes - exit with CC equal                     
         IC    R0,1(R1)            No - bump to next element on record          
         AR    R1,R0                                                            
         J     GETELA02                                                         
                                                                                
***********************************************************************         
* Initialise optimisation buffer (uses WSSVR buffer)                  *         
***********************************************************************         
                                                                                
INILBUF  NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*INILBUF'                                                      
                                                                                
T        USING TSARD,TSARABUF                                                   
         MVI   T.TSACTN,TSAINI     Set action to 'Initialise'                   
         MVI   T.TSRECI,TSRXTN+TSRWSSVR                                         
         MVI   T.TSKEYL,L'OB_KEY                                                
         LHI   R0,2*ONEK                                                        
         STCM  R0,3,T.TSBUFFL                                                   
         LHI   R0,OB_LNQ                                                        
         STCM  R0,3,T.TSRECL                                                    
         MVC   T.TSACOM,ACOMFACS                                                
         GOTOR VTSAR,T.TSARD                                                    
         JE    EXITY                                                            
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* Get a record from optimisation buffer                               *         
*                                                                     *         
* Ntry   - R1 points to caller's OB_D                                           
* Exit   - CC=Low if record not found in buffer                                 
*        CC=Equal if record found and is not posted with an error     *         
*           - record is returned in caller's OB_D                     *         
*        CC=High if record found and is posted with an error (set in  *         
*           ROUERRV)                                                  *         
***********************************************************************         
                                                                                
GETBUF   NTR1  LABEL=NO,WORK=(RC,OB_LNQ)                                        
         J     *+12                                                             
         DC    C'*GETBUF*'                                                      
                                                                                
W        USING OB_D,RC                                                          
         LR    R2,R1                                                            
P        USING OB_D,R2             R2=A(caller's OB_D)                          
         MVC   W.OB_KEY,P.OB_KEY                                                
                                                                                
T        USING TSARD,TSARABUF                                                   
         MVI   T.TSACTN,TSARDH     Set action to 'Read High'                    
         LA    R0,W.OB_D                                                        
         ST    R0,T.TSAREC         Read into acquired storage                   
         GOTOR VTSAR,T.TSARD                                                    
         JNE   EXITN               Not found/EOF                                
         DROP  T                                                                
         LA    R0,P.OB_D                                                        
         LHI   R1,OB_LNQ                                                        
         LA    RE,W.OB_D                                                        
         LHI   RF,OB_LNQ                                                        
         MVCL  R0,RE                                                            
         J     EXITY                                                            
         DROP  W,P                                                              
         EJECT                                                                  
***********************************************************************         
* check for account in Limit List                                     *         
***********************************************************************         
ADVALL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*ADVALL*'                                                      
                                                                                
         OC    CCTPID,CCTPID                                                    
         JZ    ADVALLE                                                          
         MVI   BYTE2,0                                                          
         MVI   BYTE4,C' '                                                       
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO7                                        
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO8                                        
         L     R3,AIO7                                                          
                                                                                
         MVI   BYTE1,LIDTCPJL                                                   
         CLI   RQ_ADLED,C'P'                                                    
         JE    ADVALL02                                                         
         MVI   BYTE1,LIDTNCLL                                                   
         CLI   RQ_ADLED,C'N'                                                    
         JE    ADVALL10                                                         
         CLI   RQ_ADAPL,RQ_ADORD   Don't check suppliers for orders             
         JE    ADVALL01                                                         
         MVI   BYTE1,LIDTSUPP                                                   
         CLI   RQ_ADLED,C'S'                                                    
         JE    ADVALL10                                                         
ADVALL01 MVI   BYTE1,LIDT1RAC                                                   
         CLI   RQ_ADLED,C'X'                                                    
         JNE   ADVALLE                                                          
         J     ADVALL10                                                         
                                                                                
ADVALL02 XR    R1,R1               if job check media LimList                   
         LLC   R1,PPROLEN                                                       
         LA    R1,XL#ACT(R1)                                                    
         MVC   BYTE4,0(R1)         Byte 4 has media code                        
                                                                                
ADVALL10 LA    R1,XL#ACT+L'XL#ACT-1                                             
         LA    R4,L'XL#ACT                                                      
         CLI   LDGAL1,12                                                        
         JE    ADVALL14                                                         
                                                                                
ADVALL12 CLI   0(R1),C' '                                                       
         JH    ADVALL14                                                         
         SHI   R1,1                                                             
         JCT   R4,ADVALL12                                                      
         DC    H'0'                                                             
                                                                                
         USING LLSRECD,R2                                                       
ADVALL14 SHI   R4,1                R4 = length of entry (exec)                  
         LA    R2,IOKEY                                                         
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,CCTPID                                                  
         MVC   CSVKEY1,LLSKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   LLSKEY(LLSKGRP-LLSKEY),CSVKEY1                                   
         JNE   ADVALL36                                                         
         MVI   X#LLTABS,X#LLALLQ   Default to unlimited access                  
         MVC   X#LLTABS+1(X#LLTLQ-1),X#LLTABS                                   
         J     ADVALL18                                                         
                                                                                
ADVALL16 LA    R2,IOKEY                                                         
         MVC   IOKEY,CSVKEY1       Reread sequential GrpList record             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         LHI   RF,(LLSKGRP-LLSKEY)-1                                            
         CLI   LLSKSUB,LLSKSUBQ      Test Limit list record                     
         JE    *+8                                                              
         LHI   RF,(GLSKSEQ-GLSKEY)-1 No - use  Group list length                
         BASR  RE,0                                                             
         CLC   LLSKEY(0),CSVKEY1                                                
         EX    RF,0(RE)                                                         
         JNE   ADVALL36                                                         
ADVALL18 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
         L     R2,AIO1                                                          
         MVC   CSVKEY1,0(R2)       Save the record key                          
         AHI   R2,LLSRFST-LLSRECD                                               
         USING LIDELD,R2                                                        
ADVALL20 CLI   LIDEL,0                                                          
         JE    ADVALL16            (no entry found = OK)                        
         CLI   LIDEL,RSTELQ                                                     
         JE    ADVALL34                                                         
         CLI   LIDEL,LIDELQ                                                     
         JNE   ADVALL30                                                         
         CLI   BYTE4,C' '          media limlist check                          
         JNH   *+12                No skip it                                   
         CLI   LIDTYPE,LIDTMEDL                                                 
         JE    ADVALL32                                                         
         CLC   LIDTYPE,BYTE1                                                    
         JE    ADVALL32                                                         
                                                                                
ADVALL30 XR    R0,R0                                                            
         LLC   R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     ADVALL20                                                         
                                                                                
ADVALL32 LLC   RF,LIDLN                                                         
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,R3),LIDELD                                                   
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R3,RF                                                            
         J     ADVALL30                                                         
                                                                                
         USING RSTELD,R2                                                        
ADVALL34 CLI   RSTLN,RSTLN3Q                                                    
         JL    ADVALL30                                                         
         TM    RSTACST1,RSTAJOBS                                                
         JZ    *+8                                                              
         MVI   X#LLTCPJ,X#LLNONQ                                                
         TM    RSTACST1,RSTAMED    Media code limit list                        
         JZ    *+8                                                              
         MVI   X#LLTMED,X#LLNONQ                                                
         TM    RSTACST1,RSTAETYP   Expenditure type limit list                  
         JZ    *+8                                                              
         MVI   X#LLTETY,X#LLNONQ                                                
         TM    RSTACST1,RSTA1NAC   Non-client limit list                        
         JZ    *+8                                                              
         MVI   X#LLTNC,X#LLNONQ                                                 
         TM    RSTACST1,RSTASTAF   1R costing account list                      
         JZ    *+8                                                              
         MVI   X#LLT1R,X#LLNONQ                                                 
         TM    RSTACST2,RSTASUPP   Supplier list                                
         JZ    *+8                                                              
         MVI   X#LLTSUP,X#LLNONQ                                                
         J     ADVALL30                                                         
                                                                                
ADVALL36 L     R2,AIO7                                                          
         USING LIDELD,R2                                                        
         CLI   BYTE4,C' '          media limlist check                          
         JNH   ADVALL56            No skip it and check other entries           
                                                                                
ADVALL38 CLI   LIDEL,0                                                          
         JE    ADVALL54            (no entry found = OK)                        
         CLI   LIDEL,LIDELQ                                                     
         JNE   ADVALL40                                                         
         CLI   LIDTYPE,LIDTMEDL    listing media                                
         JE    ADVALL42                                                         
                                                                                
ADVALL40 XR    R0,R0                                                            
         LLC   R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     ADVALL38                                                         
                                                                                
ADVALL42 XR    R3,R3                                                            
         LLC   R3,LIDLN                                                         
         AR    R3,R2                                                            
         LA    R1,LIDDATA                                                       
M        USING LIDDATA,R1                                                       
*&&UK*&& MVI   X#LLTMED,X#LLLSTQ                                                
                                                                                
ADVALL44 CR    R1,R3                                                            
         JNL   ADVALL54            (missing in media limlist = bad)             
         LA    RF,APPLTAB                                                       
ADVALL46 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   XL#APPL,0(RF)                                                    
         JE    ADVALL48                                                         
         LA    RF,APPLTABL(RF)                                                  
         J     ADVALL46                                                         
                                                                                
ADVALL48 LLC   RE,1(RF)                                                         
         BASR  RF,0                                                             
         TM    M.LIDLAPPL,0                                                     
         EX    RE,0(RF)                                                         
         JZ    ADVALL50                                                         
*&&US*&& MVI   X#LLTMED,X#LLLSTQ                                                
                                                                                
         CLC   BYTE4,M.LIDLMED                                                  
         JE    ADVALL56            (match on media = OK)                        
ADVALL50 AHI   R1,LIDLLN3Q                                                      
         J     ADVALL44                                                         
         DROP  M                                                                
                                                                                
ADVALL54 CLI   X#LLTMED,X#LLALLQ                                                
         JNE   ADVALLH                                                          
                                                                                
ADVALL56 L     R2,AIO7                                                          
         MVI   BYTE4,0                                                          
                                                                                
ADVALL58 CLI   LIDEL,0                                                          
         JE    ADVALL76                                                         
         CLI   LIDEL,LIDELQ                                                     
         JNE   ADVALL60                                                         
         CLC   LIDTYPE,BYTE1                                                    
         JE    ADVALL62                                                         
                                                                                
ADVALL60 LLC   R0,LIDLN                                                         
         AR    R2,R0                                                            
         J     ADVALL58                                                         
                                                                                
ADVALL62 LLC   RF,LIDITLN                                                       
         LLC   R3,LIDLN                                                         
         AR    R3,R2                                                            
         LA    R1,LIDDATA                                                       
AC       USING LIDDATA,R1                                                       
*&&UK                                                                           
         CLI   LIDTYPE,LIDTSUPP    Supplier                                     
         JNE   *+8                                                              
         MVI   X#LLTSUP,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDT1RAC    1R                                           
         JNE   *+8                                                              
         MVI   X#LLT1R,X#LLLSTQ                                                 
         CLI   LIDTYPE,LIDTCPJL    cli/pro/job                                  
         JNE   *+8                                                              
         MVI   X#LLTCPJ,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTEXPL    expenditure type                             
         JNE   *+8                                                              
         MVI   X#LLTETY,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTWCL     workcode                                     
         JNE   *+8                                                              
         MVI   X#LLTWC,X#LLLSTQ                                                 
         CLI   LIDTYPE,LIDTNCLL    non client                                   
         JNE   *+8                                                              
         MVI   X#LLTNC,X#LLLSTQ                                                 
*&&                                                                             
                                                                                
ADVALL64 CR    R1,R3                                                            
         JNL   ADVALL60                                                         
         LA    RF,APPLTAB                                                       
ADVALL66 CLI   0(RF),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   XL#APPL,0(RF)                                                    
         JE    ADVALL68                                                         
         LA    RF,APPLTABL(RF)                                                  
         J     ADVALL66                                                         
                                                                                
ADVALL68 LLC   RE,1(RF)                                                         
         BASR  RF,0                                                             
         TM    AC.LIDLAPPL,0                                                    
         EX    RE,0(RF)                                                         
         JZ    ADVALL74                                                         
*&&US                                                                           
         MVI   BYTE2,1                                                          
         CLI   LIDTYPE,LIDTSUPP    Supplier                                     
         JNE   *+8                                                              
         MVI   X#LLTSUP,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDT1RAC    1R                                           
         JNE   *+8                                                              
         MVI   X#LLT1R,X#LLLSTQ                                                 
         CLI   LIDTYPE,LIDTCPJL    cli/pro/job                                  
         JNE   *+8                                                              
         MVI   X#LLTCPJ,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTEXPL    expenditure type                             
         JNE   *+8                                                              
         MVI   X#LLTETY,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTWCL     workcode                                     
         JNE   *+8                                                              
         MVI   X#LLTWC,X#LLLSTQ                                                 
         CLI   LIDTYPE,LIDTNCLL    non client                                   
         JNE   *+8                                                              
         MVI   X#LLTNC,X#LLLSTQ                                                 
*&&                                                                             
                                                                                
         LA    RE,AC.LIDLACT+L'LIDLACT-1                                        
         LA    RF,L'LIDLACT                                                     
         CLI   LDGAL1,12                                                        
         JE    ADVALL72                                                         
                                                                                
ADVALL70 CLI   0(RE),C' '                                                       
         JH    ADVALL72                                                         
         SHI   RE,1                                                             
         JCT   RF,ADVALL70                                                      
         J     ADVALL60                                                         
                                                                                
ADVALL72 SHI   RF,1                                                             
         CR    RF,R4                                                            
         JNH   *+6                                                              
         LR    RF,R4                                                            
         BASR  R6,0                                                             
         CLC   XL#ACT(0),AC.LIDLACT                                             
         EX    RF,0(R6)                                                         
         JNE   ADVALL74                                                         
         CLI   LIDTYPE,LIDTCPJL    For client prod job need to check            
         JNE   ADVALLE             office                                       
         CLC   AC.LIDLOFF,SPACES   Do we have an office                         
         JNH   ADVALLE             Accept if not                                
         AHI   RF,1                                                             
         CLM   RF,1,PCLILEN        client level?                                
         JH    ADVALL7A            Else check product-lvl office table          
*         JNH   ADVALL7A                                                        
         CLC   AD_OFFC,AC.LIDLOFF  If we do ensure they match                   
         JE    ADVALLE                                                          
         J     ADVALL74                                                         
*                                                                               
ADVALL7A LA    RF,XL#COFFL                                                      
         LA    RE,XL#COFFL+L'XL#COFFL                                           
         CLC   0(L'LIDLOFF,RF),SPACES  Any entries in table?                    
         JH    ADVALL73                                                         
         CLC   AD_OFFC,AC.LIDLOFF  Otherwise match client office with           
         JE    ADVALLE             office in limlist                            
         J     ADVALL74                                                         
*                                                                               
ADVALL73 CR    RF,RE               Hit end of table?                            
         JNL   ADVALL74                                                         
         CLC   0(L'LIDLOFF,RF),SPACES                                           
         JNH   ADVALL74                                                         
         CLC   0(L'LIDLOFF,RF),AC.LIDLOFF    If we do ensure they match         
         JE    ADVALLE                                                          
         AHI   RF,L'LIDLOFF                                                     
         J     ADVALL73                                                         
*                                                                               
ADVALL74 LLC   RF,LIDITLN                                                       
         AR    R1,RF                                                            
         J     ADVALL64                                                         
         DROP  AC                                                               
                                                                                
ADVALL76 CLI   BYTE2,1                                                          
         JE    ADVALLL                                                          
         CLI   RQ_ADLED,C'P'                                                    
         JNE   ADVALL78                                                         
         CLI   X#LLTCPJ,X#LLALLQ                                                
         JE    ADVALLE                                                          
         J     ADVALLL                                                          
ADVALL78 CLI   RQ_ADLED,C'N'                                                    
         JNE   ADVALL80                                                         
         CLI   X#LLTNC,X#LLALLQ                                                 
         JE    ADVALLE                                                          
         J     ADVALLL                                                          
ADVALL80 CLI   RQ_ADLED,C'S'                                                    
         JNE   ADVALLL                                                          
         CLI   X#LLTSUP,X#LLALLQ                                                
         JE    ADVALLE                                                          
         J     ADVALLL                                                          
                                                                                
ADVALLE  LA    RE,1                                                             
         J     ADVALLX                                                          
ADVALLL  SR    RE,RE                                                            
         J     ADVALLX                                                          
ADVALLH  LA    RE,2                                                             
ADVALLX  CHI   RE,1                                                             
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* check for account in Approver                                       *         
***********************************************************************         
ADVAPP   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*ADVAPP*'                                                      
                                                                                
         OC    CCTPID,CCTPID                                                    
         JZ    EXITY                                                            
                                                                                
         LA    R5,DPATYPS                                                       
ADVAP02  CLI   0(R5),X'FF'         End of table                                 
         JE    EXITN                                                            
         CLC   XL#APPL,0(R5)       Match application                            
         JE    ADVAP04                                                          
         LA    R5,DPATYPSL(R5)                                                  
         J     ADVAP02                                                          
                                                                                
ADVAP04  LA    R5,1(R5)            R5=A(first application type in list)         
                                                                                
ADVAP06  XC    XL#HIGH,XL#HIGH                                                  
         MVC   HALF1,WORK            Save unit/ledger code                      
*                                                                               
         LA    R0,3                  work out level of account passed           
         LA    R3,LDGAL4                                                        
ADVAP08  LA    R2,WORK+L'ACTKUNT+L'ACTKLDG                                      
         OC    0(1,R3),0(R3)                                                    
         JZ    ADVAP10                                                          
         XR    RE,RE                                                            
         SHI   R3,1                                                             
         LLC   RE,0(R3)                                                         
         AHI   R3,1                                                             
         AR    R2,RE                                                            
         CLI   0(R2),C' '                                                       
         JH    ADVAP12                                                          
ADVAP10  SHI   R3,1                                                             
         JCT   R0,ADVAP08                                                       
                                                                                
ADVAP12  LLC   RE,0(R3)                                                         
         STC   RE,XL#LEN                                                        
         AHI   R0,1                                                             
         STC   R0,XL#LVL                                                        
                                                                                
         USING DPAPASD,R2                                                       
ADVAP14  LA    R2,IOKEY                                                         
         XC    DPAPAS,DPAPAS                                                    
         MVI   DPAPTYP,DPAPTYPQ                                                 
         MVI   DPAPSUB,DPAPSUBQ                                                 
         MVC   DPAPCPY,CUXCPY                                                   
         ZAP   DPAPXVAL,PZERO                                                   
         MVC   DPAPAPPL,0(R5)                                                   
         J     ADVAP18                                                          
                                                                                
ADVAP16  MVC   DPAPAS,CSVKEY3                                                   
ADVAP18  MVC   DPAP1RAC,SPACES                                                  
                                                                                
         LLC   RE,0(R3)                                                         
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   DPAP1RAC(0),WORK+L'ACTKUNT+L'ACTKLDG                             
         EX    RE,0(RF)                                                         
         MVC   CSVKEY3,DPAPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     ADVAP22                                                          
ADVAP20  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
*                                                                               
ADVAP22  CLC   CSVKEY3(DPAP1RAC-DPAPASD),DPAPAS                                 
         JNE   ADVAP26                                                          
                                                                                
         LLC   RE,0(R3)                                                         
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         CLC   DPAP1RAC(0),WORK+L'ACTKUNT+L'ACTKLDG                             
         EX    RE,0(RF)                                                         
         JNE   ADVAP26                                                          
*                                                                               
         CLI   XL#HIGH,YESQ                                                     
         JNE   ADVAP24                                                          
         AHI   RE,1                                                             
         LA    RF,L'DPAP1RAC                                                    
         SR    RF,RE               RF=LENGTH OF REMAINING ACCOUNT FIELD         
         LA    R4,DPAP1RAC                                                      
         AR    R4,RE               R4=A(NEXT LEVEL DOWN ON ACCOUNT)             
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         CLC   0(0,R4),SPACES      IS ACCOUNT READ LOWER LEVEL                  
         EX    RF,0(R1)                                                         
         JNE   ADVAP26             YES - NOT INTERESTED                         
                                                                                
ADVAP24  CLC   DPAPPIDB,CCTPID                                                  
         JE    EXITY                                                            
         CLI   XL#LVL,4            did we start with the lowest level           
         JE    ADVAP28             yes - go to next appl type                   
         J     ADVAP20                                                          
         DROP  R2                                                               
*                                                                               
ADVAP26  SHI   R3,1                                                             
         MVI   XL#HIGH,YESQ                                                     
         JCT   R0,ADVAP16          Read next level down                         
ADVAP28  LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'         Have we reached end of applications          
         JNE   ADVAP06             No                                           
         J     EXITN               Yes                                          
*                                                                               
         EJECT                                                                  
**********************************************************************          
* check connected person is an approver for ledger 1R -              *          
**********************************************************************          
VALAPP   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*VALAPP*'                                                      
                                                                                
         OC    CCTPID,CCTPID                                                    
         JZ    EXITN                                                            
         CLC   PRODUL,0(R1)                                                     
         JNE   VALAP02                                                          
         MVI   BYTE3,LIDTAPSJ      Client product job account                   
         J     VALAP10                                                          
*                                                                               
VALAP02  CLC   =C'1N',0(R1)                                                     
         JNE   VALAP04                                                          
         MVI   BYTE3,LIDTAP1N      Non client account                           
         J     VALAP10                                                          
*                                                                               
VALAP04  CLC   =C'1R',0(R1)                                                     
         JNE   EXITY                                                            
         MVI   BYTE3,LIDTAP1R      Approval 1R costing account                  
         CLI   XL#APPL,RQ_ADTIM    TIME AND EXPENSE  ONLY                       
         JE    *+12                                                             
         CLI   XL#APPL,RQ_ADEXP                                                 
         JNE   EXITN                                                            
*                                                                               
         USING APPRECD,R2                                                       
VALAP10  LA    R2,IOKEY                                                         
         XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ                                                 
         MVI   APPKSUB,APPKSUBQ                                                 
         MVC   APPKCPY,CUXCPY                                                   
         MVC   APPKPIDB,CCTPID                                                  
         MVC   CSVKEY3,APPKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     VALAP14                                                          
VALAP12  GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
*                                                                               
VALAP14  CLC   CSVKEY3(APPKSEQ-APPRECD),APPKEY                                  
*&&UK*&& JNE   EXITN                                                            
*&&US*&& JNE   EXITY                                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         XR    R1,R1                                                            
         LA    R3,APPRFST                                                       
         USING LIDELD,R3                                                        
VALAP16  CLI   LIDEL,0                                                          
         JE    VALAP12                                                          
         CLI   LIDEL,LIDELQ                                                     
         JE    VALAP20                                                          
VALAP18  IC    R1,LIDLN                                                         
         AR    R3,R1                                                            
         J     VALAP16                                                          
*                                                                               
VALAP20  CLC   LIDTYPE,BYTE3                                                    
         JNE   VALAP18                                                          
         CLI   LIDLN,LIDDATA-LIDELD                                             
         JNH   VALAP18                                                          
         CLI   BYTE3,LIDTAP1N      Approval 1N account                          
         JE    *+20                                                             
         CLI   BYTE3,LIDTAPSJ      Approval SJ account                          
         JE    *+12                                                             
         CLI   BYTE3,LIDTAP1R      Approval 1R costing account                  
         JNE   EXITY                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LIDLN                                                         
         LA    RE,LIDELD(RE)                                                    
         IC    RF,LIDITLN          AND GET LENGTH OF SUB ELEMENT                
         LA    R4,LIDDATA          LOOP THROUGH SUB ELEMENTS                    
                                                                                
VALAP30  CR    R4,RE               END OF ELEMENT?                              
         JNL   VALAP18                                                          
         CLI   BYTE3,LIDTAPSJ      Approval SJ account                          
         JE    VALAPP32                                                         
         CLI   BYTE3,LIDTAP1N      Approval 1N account                          
         JE    VALAPP32                                                         
         CLI   XL#APPL,RQ_ADTIM                                                 
         JNE   *+12                                                             
         TM    0(R4),LIDAPDTI      TIME                                         
         JO    EXITY                                                            
         CLI   XL#APPL,RQ_ADEXP                                                 
         JNE   VALAPP40                                                         
         TM    0(R4),LIDAPDEX+LIDAPDED+LIDAPDEF  EXPENSES                       
         JNZ   EXITY                                                            
         J     VALAPP40                                                         
*                                                                               
VALAPP32 CLI   XL#APPL,RQ_ADTIM                                                 
         JNE   *+12                                                             
         TM    0(R4),LIDATIME      TIME                                         
         JNZ   EXITY                                                            
         CLI   XL#APPL,RQ_ADEXP                                                 
         JNE   *+12                                                             
         TM    0(R4),LIDAEXPN      EXPENSE                                      
         JNZ   EXITY                                                            
         CLI   XL#APPL,RQ_ADJOB                                                 
         JNE   *+12                                                             
         TM    0(R4),LIDAJOBY+LIDAJOBD JOB                                      
         JNZ   EXITY                                                            
         CLI   XL#APPL,RQ_ADEST                                                 
         JNE   *+12                                                             
         TM    0(R4),LIDAESTY+LIDAESTD JOB                                      
         JNZ   EXITY                                                            
         CLI   XL#APPL,RQ_ADORD                                                 
         JNE   *+12                                                             
         TM    0(R4),LIDAORDY+LIDAORDD JOB                                      
         JNZ   EXITY                                                            
         J     VALAPP40                                                         
*                                                                               
VALAPP40 AR    R4,RF               NEXT SUB ELEMENT                             
         J     VALAP30                                                          
         DROP  R2,R3                                                            
                                                                                
         EJECT                                                                  
**********************************************************************          
* Special code for German special characters (Umlaute)               *          
**********************************************************************          
UMLAUT   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*UMLAUT*'                                                      
*&&UK                                                                           
         CLI   CUCTRY,CTRYGER      (from GESRCHPARS)                            
         JNE   UMLAUTX                                                          
         XC    CSVKEY1,CSVKEY1                                                  
         XC    CSVKEY2,CSVKEY2                                                  
         XC    CSVKEY3,CSVKEY3                                                  
         MVC   CSVKEY1(L'RQ_SRCW1),RQ_SRCW1                                     
         MVC   CSVKEY2(L'RQ_SRCW2),RQ_SRCW2                                     
         MVC   CSVKEY3(L'RQ_SRCW3),RQ_SRCW3                                     
                                                                                
         XR    R4,R4                                                            
                                                                                
UMLAUT05 LR    R2,R4                                                            
         MHI   R2,L'CSVKEY1                                                     
         LA    R2,CSVKEY1(R2)                                                   
         LA    R3,L'RQ_SRCW1                                                    
                                                                                
UMLAUT10 LA    RE,=C'AE'           check for special character                  
         CLI   0(R2),X'4A'                                                      
         JE    UMLAUT15                                                         
         LA    RE,=C'OE'                                                        
         CLI   0(R2),X'E0'                                                      
         JE    UMLAUT15                                                         
         LA    RE,=C'UE'                                                        
         CLI   0(R2),X'5A'                                                      
         JE    UMLAUT15                                                         
         LA    RE,=C'SS'                                                        
         CLI   0(R2),X'A1'                                                      
         JNE   UMLAUT20                                                         
                                                                                
UMLAUT15 MVC   TEMP2(L'RQ_SRCW1),1(R2)                                          
         MVC   0(2,R2),0(RE)                                                    
         MVC   2(L'RQ_SRCW1,R2),TEMP2                                           
         J     UMLAUT05            start again after substitution               
                                                                                
UMLAUT20 AHI   R2,1                                                             
         JCT   R3,UMLAUT10                                                      
         AHI   R4,1                                                             
         CHI   R4,3                                                             
         JL    UMLAUT05                                                         
                                                                                
         MVC   RQ_SRCW1,CSVKEY1                                                 
         MVC   RQ_SRCW2,CSVKEY2                                                 
         MVC   RQ_SRCW3,CSVKEY3                                                 
*&&                                                                             
UMLAUTX  J     EXIT                                                             
***********************************************************************         
* Add a record to approver buffer/local supplier buffer               *         
*                                                                     *         
* Ntry:- R1 points to caller's APETABD/supplier record                *         
***********************************************************************         
                                                                                
ADDBUF   NTR1  LABEL=NO                                                         
         J     *+12                                                             
         DC    C'*ADDBUF*'                                                      
                                                                                
T        USING TSARD,TSARABUF                                                   
         MVI   T.TSACTN,TSAADD     Set action to 'Add'                          
         ST    R1,T.TSAREC                                                      
         GOTOR VTSAR,T.TSARD                                                    
         JE    ADDBUFX                                                          
         TM    T.TSERRS,TSEDUP                                                  
         JNZ   ADDBUFX                                                          
         DC    H'0'                                                             
ADDBUFX  J     EXIT                                                             
         DROP  T                                                                
         EJECT                                                                  
* *********************************************************************         
* Set SJ office code                                                  *         
***********************************************************************         
*                                                                               
SSJOFF   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*SSJOFF*'                                                      
                                                                                
         MVC   XL#COFF,SPACES                                                   
         CLC   PRODUL,TEMP2                                                     
         JNE   SSJOFFX                                                          
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
                                                                                
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ACTKACT(0),TEMP2+2                                               
         EX    RE,0(RF)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SSJOFFX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SSJOFFX                                                          
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R2)                                           
         USING PPRELD,R3                                                        
         XR    R0,R0                                                            
SSJOFF1  CLI   PPREL,0                                                          
         JE    SSJOFF3                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    SSJOFF2                                                          
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     SSJOFF1                                                          
                                                                                
SSJOFF2  MVC   XL#COFF,PPRGAOFF                                                 
         OC    XL#COFF,SPACES                                                   
                                                                                
SSJOFF3  LA    R2,IOKEY                                                         
         LLC   RE,PPROLEN                                                       
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ACTKACT(0),TEMP2+2                                               
         EX    RE,0(RF)                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SSJOFFX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SSJOFFX                                                          
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R2)                                           
         XR    R0,R0                                                            
SSJOFF4  CLI   PPREL,0                                                          
         JE    SSJOFF6                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    SSJOFF5                                                          
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     SSJOFF4                                                          
                                                                                
SSJOFF5  CLC   PPRGAOFF,SPACES                                                  
         JNH   SSJOFF6                                                          
         MVC   XL#COFF,PPRGAOFF                                                 
         OC    XL#COFF,SPACES                                                   
                                                                                
SSJOFF6  LA    R2,IOKEY                                                         
         MVC   ACTKACT,TEMP2+2                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SSJOFFX                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SSJOFFX                                                          
                                                                                
         L     R2,AIO1                                                          
         LA    R3,ACTRFST-ACTRECD(R2)                                           
         XR    R0,R0                                                            
SSJOFF7  CLI   PPREL,0                                                          
         JE    SSJOFFX                                                          
         CLI   PPREL,PPRELQ                                                     
         JE    SSJOFF8                                                          
         IC    R0,PPRLN                                                         
         AR    R3,R0                                                            
         J     SSJOFF7                                                          
                                                                                
SSJOFF8  CLC   PPRGAOFF,SPACES                                                  
         JNH   SSJOFFX                                                          
         MVC   XL#COFF,PPRGAOFF                                                 
         OC    XL#COFF,SPACES                                                   
                                                                                
SSJOFFX  J     EXIT                                                             
         DROP  R2,R3                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* Get list of product offices into XL#COFFL                           *         
* AIO1 has client record                                              *         
***********************************************************************         
         SPACE 1                                                                
GETPOF   NTR1                                                                   
         L     R4,AIO1                                                          
         LA    R4,ACTRFST-ACTRECD(R4)                                           
         LA    R2,XL#COFFL                                                      
*                                                                               
         USING LIDELD,R4                                                        
GETPOF02 CLI   LIDEL,0                                                          
         JE    GETPOFX             No valid office code found                   
         CLI   LIDEL,LIDELQ                                                     
         JNE   GETPOF04                                                         
         CLI   LIDTYPE,LIDTPOFC      Product office codes                       
         JE    GETPOF06                                                         
GETPOF04 LLC   R0,LIDLN                                                         
         AR    R4,R0                                                            
         J     GETPOF02                                                         
*                                                                               
GETPOF06 LA    RF,XL#COFFL+L'XL#COFFL Check area is not full up                 
         CR    R2,RF                                                            
         JNL   GETPOFX                                                          
         LLC   RF,LIDLN                                                         
         SHI   RF,(LIDDATA-LIDELD)+1 RF=Length of LIDDATA                       
         BASR  R1,0                                                             
         MVC   0(0,R2),LIDDATA                                                  
         EX    RF,0(R1)                                                         
         AHI   RF,1                                                             
         AR    R2,RF                                                            
         J     GETPOF04                                                         
*                                                                               
GETPOFX  J     EXITY                                                            
         DROP  R4                                                               
*                                                                               
* Get limit list data                                                           
* Etry: R1 - list type for MCS record                                           
* Exit: ELEMENT (LIDTMED,LIDTEXPD,LIDTWKCD) or LIACBLK contains limlist         
* This routine uses BYTE3 and BYTE4                                             
                                                                                
GETLMT   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*GETLMT*'                                                      
*                                                                               
         CLI   XL#APPL,RQ_MAINT                                                 
         JE    EXITN                                                            
         STC   R1,BYTE3            Save MCS list type                           
*                                                                               
         L     R0,AIO8                                                          
         ST    R0,XL#ADR2                                                       
         L     R0,AIO7                                                          
         MVI   BYTE4,C'Y'          Store limit list in AIO7                     
         CLI   BYTE3,LIDTMEDL      Media code record                            
         JE    GETLMT02                                                         
         CLI   BYTE3,LIDTWCL       Work code record                             
         JE    GETLMT02                                                         
*                                                                               
         MVI   BYTE4,0                                                          
         LAY   R0,LIACBLK                                                       
GETLMT02 ST    R0,XL#ADR1                                                       
*                                                                               
         OC    CCTPID,CCTPID                                                    
         JZ    EXITN                                                            
*                                                                               
         USING LLSRECD,R2          Read list PID record for this person         
         LA    R2,IOKEY                                                         
         XC    LLSKEY,LLSKEY                                                    
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         MVC   LLSKCPY,CUXCPY                                                   
         MVC   LLSKPIDB,CCTPID                                                  
         MVC   CSVKEY3,LLSKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         CLC   IOKEY(LLSKGRP-LLSRECD),CSVKEY3                                   
         JNE   EXITN                                                            
         NI    XL#GLIND,FF-XL#GLIFQ                                             
         TM    XL#GLIND,XL#GLIPQ   Processed before?                            
         JNZ   GETLMT06                                                         
         OI    XL#GLIND,XL#GLIPQ+XL#GLIFQ                                       
         MVI   X#LLTABS,X#LLALLQ   Default to unlimited access if               
         MVC   X#LLTABS+1(X#LLTLQ-1),X#LLTABS we have limit list rec            
         OI    X#LLIND,X#LLINI                                                  
         J     GETLMT06                                                         
GETLMT04 LA    R2,IOKEY                                                         
         MVC   IOKEY,CSVKEY3       Reread sequential GrpList record             
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
         LHI   RF,(LLSKGRP-LLSKEY)-1                                            
         CLI   LLSKSUB,LLSKSUBQ      Test Limit list record                     
         JE    *+8                                                              
         LHI   RF,(GLSKSEQ-GLSKEY)-1 No - use  Group list length                
         BASR  R1,0                                                             
         CLC   LLSKEY(0),CSVKEY3                                                
         EX    RF,0(R1)                                                         
         JNE   GETLMT48                                                         
GETLMT06 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                Bad master record                            
         DROP  R2                                                               
*                                                                               
         L     R1,AIO1                                                          
         MVC   CSVKEY3,0(R1)                                                    
         LA    R1,LLSRFST-LLSRECD(R1)                                           
         USING LIDELD,R1                                                        
*                                                                               
GETLMT10 CLI   LIDEL,0                                                          
         JE    GETLMT04                                                         
         CLI   LIDEL,RSTELQ                                                     
         JE    GETLMT34                                                         
         CLI   LIDEL,LIDELQ                                                     
         JNE   GETLMT12                                                         
         CLC   LIDTYPE,BYTE3                                                    
         JE    GETLMT14                                                         
*                                                                               
GETLMT12 XR    RE,RE                                                            
         IC    RE,LIDLN                                                         
         AR    R1,RE                                                            
         J     GETLMT10            Get next                                     
*                                                                               
GETLMT14 XR    RE,RE                                                            
         IC    RE,LIDLN                                                         
         SHI   RE,(LIDDATA-LIDELD)+1                                            
         JM    GETLMT12            No client code                               
*&&UK                                                                           
         CLI   LIDTYPE,LIDTMEDL    media list                                   
         JNE   *+8                                                              
         MVI   X#LLTMED,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTSUPP    supplier list                                
         JNE   *+8                                                              
         MVI   X#LLTSUP,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDT1RAC    1R account                                   
         JNE   *+8                                                              
         MVI   X#LLT1R,X#LLLSTQ                                                 
         CLI   LIDTYPE,LIDTCPJL    cli/pro/job list                             
         JNE   *+8                                                              
         MVI   X#LLTCPJ,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTEXPL    expenditure type list                        
         JNE   *+8                                                              
         MVI   X#LLTETY,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTWCL     workcode list                                
         JNE   *+8                                                              
         MVI   X#LLTWC,X#LLLSTQ                                                 
         CLI   LIDTYPE,LIDTNCLL    non client code list                         
         JNE   *+8                                                              
         MVI   X#LLTNC,X#LLLSTQ                                                 
*&&                                                                             
         XR    R3,R3                                                            
         IC    R3,LIDLN                                                         
         AR    R3,R1                                                            
         LA    R2,LIDDATA                                                       
M        USING LIDDATA,R2                                                       
                                                                                
GETLMT24 CR    R2,R3                                                            
         JNL   GETLMT12            (missing in media limlist = bad)             
         LA    RE,APPLTAB                                                       
GETLMT26 CLI   0(RE),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLC   XL#APPL,0(RE)                                                    
         JE    GETLMT28                                                         
         LA    RE,APPLTABL(RE)                                                  
         J     GETLMT26                                                         
                                                                                
GETLMT28 LLC   RF,1(RE)                                                         
         BASR  R4,0                                                             
         TM    M.LIDLAPPL,0                                                     
         EX    RF,0(R4)                                                         
         JZ    GETLMT32                                                         
*&&US                                                                           
         CLI   LIDTYPE,LIDTMEDL    media list                                   
         JNE   *+8                                                              
         MVI   X#LLTMED,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTSUPP    supplier list                                
         JNE   *+8                                                              
         MVI   X#LLTSUP,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDT1RAC    1R account                                   
         JNE   *+8                                                              
         MVI   X#LLT1R,X#LLLSTQ                                                 
         CLI   LIDTYPE,LIDTCPJL    cli/pro/job list                             
         JNE   *+8                                                              
         MVI   X#LLTCPJ,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTEXPL    expenditure type list                        
         JNE   *+8                                                              
         MVI   X#LLTETY,X#LLLSTQ                                                
         CLI   LIDTYPE,LIDTWCL     workcode list                                
         JNE   *+8                                                              
         MVI   X#LLTWC,X#LLLSTQ                                                 
         CLI   LIDTYPE,LIDTNCLL    non client code list                         
         JNE   *+8                                                              
         MVI   X#LLTNC,X#LLLSTQ                                                 
*&&                                                                             
         CLI   LIDTYPE,LIDTSUPP                                                 
         JNE   GETLMT2A                                                         
         CLI   M.LIDLSULA,C' '     suppliers start w/ optional office           
         JE    GETLMT32                                                         
         L     RF,XL#ADR1          List area                                    
         LLC   RE,LIDITLN                                                       
         SHI   RE,1+L'LIDLAPPL+L'LIDLAPP2+L'LIDLOFFC                            
         BASR  R4,0                                                             
         MVC   0(0,RF),M.LIDLSULA  Save account codes                           
         EX    RE,0(R4)                                                         
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         ST    RF,XL#ADR1                                                       
         J     GETLMT32                                                         
*                                                                               
GETLMT2A CLI   M.LIDLACT,C' '      Any account?                                 
         JE    GETLMT30                                                         
         L     RF,XL#ADR1          List area                                    
         LLC   RE,LIDITLN                                                       
         SHI   RE,1+L'LIDLAPPL+L'LIDLAPP2                                       
         BASR  R4,0                                                             
         MVC   0(0,RF),M.LIDLACT  Save client codes                             
         EX    RE,0(R4)                                                         
         AHI   RE,1                                                             
         AR    RF,RE                                                            
         ST    RF,XL#ADR1                                                       
         J     GETLMT32                                                         
                                                                                
GETLMT30 L     RF,XL#ADR2          List area                                    
         MVC   0(L'LIDLOFF,RF),M.LIDLOFF  Save client codes                     
         LA    RF,L'LIDLOFF(RF)                                                 
         ST    RF,XL#ADR2                                                       
                                                                                
GETLMT32 LLC   RE,LIDITLN                                                       
         AR    R2,RE                                                            
         J     GETLMT24                                                         
         DROP  M                                                                
*                                                                               
         USING RSTELD,R1                                                        
GETLMT34 TM    XL#GLIND,XL#GLIFQ                                                
         JZ    GETLMT12                                                         
         CLI   RSTLN,RSTLN3Q                                                    
         JL    GETLMT12                                                         
         TM    RSTACST1,RSTAJOBS                                                
         JZ    GETLMT36                                                         
         CLI   X#LLTCPJ,X#LLALLQ                                                
         JNE   GETLMT36                                                         
         MVI   X#LLTCPJ,X#LLNONQ                                                
GETLMT36 TM    RSTACST1,RSTAMED    Media code limit list                        
         JZ    GETLMT38                                                         
         CLI   X#LLTMED,X#LLALLQ                                                
         JNE   GETLMT38                                                         
         MVI   X#LLTMED,X#LLNONQ                                                
GETLMT38 TM    RSTACST1,RSTAETYP   Expenditure type limit list                  
         JZ    GETLMT40                                                         
         CLI   X#LLTETY,X#LLALLQ                                                
         JNE   GETLMT40                                                         
         MVI   X#LLTETY,X#LLNONQ                                                
GETLMT40 TM    RSTACST1,RSTA1NAC   Non-client limit list                        
         JZ    GETLMT42                                                         
         CLI   X#LLTNC,X#LLALLQ                                                 
         JNE   GETLMT42                                                         
         MVI   X#LLTNC,X#LLNONQ                                                 
GETLMT42 TM    RSTACST1,RSTASTAF   1R costing account list                      
         JZ    GETLMT44                                                         
         CLI   X#LLT1R,X#LLALLQ                                                 
         JNE   GETLMT44                                                         
         MVI   X#LLT1R,X#LLNONQ                                                 
GETLMT44 TM    RSTACST1,RSTAWC     Workcode list                                
         JZ    GETLMT46                                                         
         CLI   X#LLTWC,X#LLALLQ                                                 
         JNE   GETLMT46                                                         
         MVI   X#LLTWC,X#LLNONQ                                                 
GETLMT46 TM    RSTACST2,RSTASUPP   Supplier list                                
         JZ    GETLMT12                                                         
         CLI   X#LLTSUP,X#LLALLQ                                                
         JNE   GETLMT12                                                         
         MVI   X#LLTSUP,X#LLNONQ                                                
         J     GETLMT12                                                         
*                                                                               
GETLMT48 CLI   BYTE4,C'Y'                                                       
         JNE   GETLMT50                                                         
         L     R1,AIO7                                                          
         OC    0(L'ELEMENT,R1),0(R1)                                            
         JZ    EXITN                                                            
         J     EXITY                                                            
*                                                                               
GETLMT50 LAY   R4,LIACBLK                                                       
         OC    0(L'ACTKACT,R4),0(R4)        Any limit list                      
         JZ    EXITN                        No                                  
         J     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
* Sort Client list                                                              
SRTCLI   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*SRTCLI*'                                                      
                                                                                
         LAY   R2,LIACBLK          count # of entries and make them             
         XR    R3,R3                                                            
         LLC   R0,PCLILEN                                                       
         LR    R1,R0                                                            
         SHI   R1,1                                                             
         LA    RF,L'ACTKACT                                                     
         SR    RF,R0                                                            
         SHI   RF,1                                                             
                                                                                
SRTCLI05 OC    0(L'ACTKACT,R2),0(R2)                                            
         JZ    SRTCLI20                                                         
         LR    RE,R0                                                            
         AR    RE,R2                                                            
         BASR  R4,0                                                             
         MVC   0(0,RE),SPACES                                                   
         EX    RF,0(R4)                                                         
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  R4,0                                                             
         CLC   DUB1(L'RQ_ALCLI),SPACES                                          
         EX    RE,0(R4)                                                         
         JNH   SRTCLI10                                                         
         BASR  R4,0                                                             
         CLC   DUB1(0),0(R2)                                                    
         EX    R1,0(R4)                                                         
         JNE   SRTCLI15                                                         
                                                                                
SRTCLI10 AHI   R3,1                                                             
                                                                                
SRTCLI15 AHI   R2,L'ACTKACT+L'LIDASJOF                                          
         J     SRTCLI05                                                         
                                                                                
SRTCLI20 LTR   R3,R3                                                            
         JZ    SRTCLIX                                                          
         LAY   R2,LIACBLK          count # of entries and make them             
         GOTO1 VXSORT,DMCB,(R2),(R3),L'ACTKACT+L'LIDASJOF,             +        
               L'ACTKACT+L'LIDASJOF,0                                           
         LTR   R3,R3                                                            
                                                                                
SRTCLIX  J     EXIT                                                             
                                                                                
         EJECT                                                                  
* Filter on search directory key                                                
                                                                                
FLTSRC   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*FLTSRC*'                                                      
         USING SRCRECD,R2                                                       
         LA    R2,IOKEY                                                         
         CLI   XL#SRAL,FF          ANY ACCOUNT FILTER?                          
         JE    FLTSRC02                                                         
                                                                                
         LLC   RF,XL#SRAL                                                       
*&&US                                                                           
         CLC   RQ_SRCUL,=C'1R'     get actual 1R lvl len for comp               
         JNE   *+12                                                             
         LA    R1,ONERL1L                                                       
         LA    R0,4                                                             
         CLC   RQ_SRCUL,=C'SJ'     get actual SJ lvl len for comp               
         JNE   FLTSRC01                                                         
         LA    R1,PCLILEN                                                       
         LA    R0,3                                                             
         LLC   RE,0(R1)                                                         
         AHI   RE,-1               For Executable length                        
         CR    RF,RE                                                            
         JNH   *+12                                                             
         LA    R1,1(R1)                                                         
         JCT   R0,*-20                                                          
         LR    RF,RE                                                            
*&&                                                                             
FLTSRC01 BASR  R1,0                                                             
         CLC   RQ_SRCAC(0),SRCKACT                                              
         EX    RF,0(R1)                                                         
         JNE   EXITN                                                            
*                                                                               
FLTSRC02 CLI   XL#CLVL,0           specific level required?                     
         JE    FLTSRC06                                                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         IC    RF,LDGAL1                                                        
         CLI   XL#CLVL,1                                                        
         JE    FLTSRC04                                                         
         IC    RE,LDGAL1                                                        
         IC    RF,LDGAL2                                                        
         CLI   XL#CLVL,2                                                        
         JE    FLTSRC04                                                         
         IC    RE,LDGAL2                                                        
         IC    RF,LDGAL3                                                        
         CLI   XL#CLVL,3                                                        
         JE    FLTSRC04                                                         
         IC    RE,LDGAL3                                                        
FLTSRC04 LA    RE,SRCKACT(RE)                                                   
         LA    RF,SRCKACT(RF)                                                   
         CLI   0(RE),C' '          1st char of lvl must JE set                  
         JNH   EXITN                                                            
         CLI   XL#CLVL,4                                                        
         JE    FLTSRC06                                                         
         CLI   0(RF),C' '          1st char of next lvl mustn't JE set          
         JH    EXITN                                                            
*                                                                               
FLTSRC06 XC    XL#AOFF,XL#AOFF     set account's office if possible             
         CLI   LDGAOP,0                                                         
         JE    EXITY                                                            
         CLI   LDGAOP,LDGOKEY                                                   
         JH    FLTSRC08                                                         
         XR    RE,RE                                                            
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         AHI   RE,1                                                             
         XR    RF,RF                                                            
         IC    RF,LDGAOP                                                        
         LA    RF,SRCKACT-1(RF)                                                 
         BASR  R1,0                                                             
         MVC   XL#AOFF(0),0(RF)                                                 
         EX    RE,0(R1)                                                         
         J     FLTSRC12                                                         
                                                                                
FLTSRC08 CLI   LDGAOP,LDGOPROF                                                  
         JNE   FLTSRC10                                                         
         MVC   XL#AOFF,SRCKSOFF                                                 
         J     FLTSRC12                                                         
*                                                                               
FLTSRC10 CLI   LDGAOP,LDGOOFLS                                                  
         JNE   EXITY                                                            
         MVC   XL#AOFF,SRCKACT                                                  
*                                                                               
FLTSRC12 OC    XL#AOFF,SPACES                                                   
         OC    XL#COFF,XL#COFF                                                  
         JZ    FLTSRC14                                                         
         CLC   XL#AOFF,XL#COFF                                                  
         JNE   EXITN                                                            
*                                                                               
FLTSRC14 DS    0H                                                               
*&&US*&& CLI   XL#APPL,RQ_ADEXP    searching from Expense module ?              
*&&US*&& JNE   EXITY               USA does not have srch rec offices           
         CLI   CUACCS,0            LIMIT ACCESS?                                
         JE    EXITY                                                            
         USING OFFALD,R1                                                        
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,XL#AOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
*&&UK                                                                           
         CLI   LDGAOP,LDGOOFLS                                                  
         JNE   *+8                                                              
         OI    OFFACTRL,OFFOFLIS                                                
*&&                                                                             
         GOTO1 VOFFAL              VALIDATE OFFICE                              
         JE    EXITY                                                            
         CLC   RQ_SRCUL,PRODUL     SJ ledger?                                   
         JNE   EXITN                                                            
*&&US*&& J     EXITY       US search does not have srcksoff populated.          
         CLI   XL#CLVL,2                                                        
         JH    EXITN                                                            
         DROP  R1                                                               
*                                                                               
         MVC   CSVKEY2,IOKEY       SAVE CURRENT IOKEY                           
         USING ACTRECD,R4                                                       
         LA    R4,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         MVC   ACTKACT(L'SRCKACT),CSVKEY2+SRCKACT-SRCRECD                       
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO2'                               
         JE    *+6                                                              
         DC    H'0'              FILE ERROR                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO2'                              
         JE    *+6                                                              
         DC    H'0'              FILE ERROR                                     
         L     R4,AIO2                                                          
         LA    R4,ACTRFST-ACTRECD(R4)                                           
         GOTOR CHKOFF                                                           
         JE    FLTSRC16                                                         
         MVC   IOKEY,CSVKEY2                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1' RE-ESTABLISH SEQUENCE         
         J     EXITN               No valid office code found                   
FLTSRC16 MVC   IOKEY,CSVKEY2                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1' RE-ESTABLISH SEQUENCE         
         DROP  R4,R2                                                            
         J     EXITY                                                            
                                                                                
         EJECT                                                                  
                                                                                
* Put search directory key into table (sorted)                                  
                                                                                
PUTSRC   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*PUTSRC*'                                                      
         USING SRCRECD,R2                                                       
         USING WORDTD,R3                                                        
         LA    R2,IOKEY                                                         
         LA    R3,TEMP2            BUILD table entry                            
         MVC   WORDAC,SRCKACT                                                   
         MVC   WORDDA,SRCKDA                                                    
         MVC   WORDWS,XL#CWNO                                                   
         MVC   WORDOFF,XL#AOFF     SAVE OFFICE                                  
*                                                                               
         L     RE,AELEPTRR         point to last entry                          
TBL      USING WORDTD,RE                                                        
PUTSRC10 CLC   WORDAC,TBL.WORDAC   match on entry?                              
         JL    PUTSRC20                                                         
         JH    PUTSRC12                                                         
         OC    TBL.WORDWS,WORDWS   equal then update entry and exit             
         J     EXITY                                                            
PUTSRC12 OC    TBL.WORDTD(WORDLQ),TBL.WORDTD                                    
         JZ    *+8                                                              
         AHI   RE,WORDLQ           add at end of table                          
         MVC   TBL.WORDTD(WORDLQ),WORDTD                                        
         ST    RE,AELEPTRR                                                      
         J     PUTSRC90                                                         
         DROP  TBL                                                              
TBL      USING WORDTD,RE                                                        
PUTSRC20 LAY   RE,GENAREA          point to first entry                         
         L     R1,AELEPTRR                                                      
PUTSRC22 CR    RE,R1                                                            
         JNH   *+6                                                              
         DC    H'0'                fault in logic here                          
         CLC   TBL.WORDAC,WORDAC   compare current with new entry               
         JE    PUTSRC24                                                         
         JH    PUTSRC26                                                         
         AHI   RE,WORDLQ           if low go to next entry                      
         J     PUTSRC22                                                         
PUTSRC24 OC    TBL.WORDWS,WORDWS   equal then update entry and exit             
         J     EXITY                                                            
PUTSRC26 LR    RF,R1               then move table                              
         AHI   RF,WORDLQ                                                        
         ST    RF,AELEPTRR         but first incr. last entry pointer           
         XR    RE,R1               swap R1 and RE                               
         XR    R1,RE                                                            
         XR    RE,R1               (RE loops, R1 points to current)             
PUTSRC28 CR    R1,RE               move entries until current reached           
         JH    PUTSRC30                                                         
         MVC   TBL.WORDTD+WORDLQ(WORDLQ),TBL.WORDTD                             
         SHI   RE,WORDLQ                                                        
         J     PUTSRC28                                                         
PUTSRC30 LR    RE,R1                                                            
         MVC   TBL.WORDTD(WORDLQ),WORDTD                                        
         J     PUTSRC90                                                         
         DROP  TBL                                                              
*                                                                               
PUTSRC90 LH    RF,XL#SRCT          ensure not too many items                    
         AHI   RF,1                                                             
         STH   RF,XL#SRCT                                                       
         CHI   RF,XL#SMXQ                                                       
         JH    EXITN                                                            
         J     EXITY                                                            
         DROP  R2,R3                                                            
                                                                                
* Get search directory key from table                                           
                                                                                
GETSRC   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*GETSRC*'                                                      
         USING WORDTD,R2                                                        
         L     R1,AELEPTRR        check end of table                            
         L     R2,DUB2            next entry                                    
         CR    R2,R1                                                            
         JH    GETSRCN                                                          
         LR    RF,R2                                                            
         AHI   RF,WORDLQ                                                        
         ST    RF,DUB2                                                          
*                                                                               
GETSRCY  CR    RE,RE                                                            
         J     *+6                                                              
                                                                                
GETSRCN  LTR   RE,RE                                                            
         XIT1  REGS=(R2)                                                        
         DROP  R2                                                               
                                                                                
***********************************************************************         
* TEST AIO1 RECORD AND EXTRACT NAME INTO TEMP2 PLUS FILTER OUT DRAFT  *         
* AND CLOSED ACCOUNTS                                                 *         
***********************************************************************         
TSTSET   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*TSTSET*'                                                      
         USING ACTRECD,R3                                                       
         XC    BYTE3,BYTE3                                                      
         L     R3,AIO1                                                          
         MVC   TEMP2(36),SPACES                                                 
         MVI   HALF1,FF                                                         
         MVI   HALF2,FF                                                         
*                                                                               
TSTSET04 CLI   RQ_SRCLO,C'O'       ONLY                                         
         JNE   TSTSET06                                                         
         TM    ACTRSTAT,ACTSCLOS   ONLY CLOSED ACCOUNTS                         
         JZ    EXITN                                                            
         J     TSTSET16                                                         
*                                                                               
TSTSET06 CLI   RQ_SRCLO,NOQ        NO                                           
         JE    *+12                                                             
         CLI   RQ_SRCLO,C' '       BLANK MEANS NO                               
         JH    TSTSET08                                                         
         TM    ACTRSTAT,ACTSCLOS   DON'T WANT CLOSED THEN                       
         JNZ   EXITN                                                            
*                                                                               
TSTSET08 CLI   RQ_SRILA,C'O'       ONLY                                         
         JNE   TSTSET10                                                         
         TM    ACTRSTAT,ACTSLOCK   ONLY LOCKED ACCOUNTS                         
         JZ    EXITN                                                            
         J     TSTSET16                                                         
*                                                                               
TSTSET10 CLI   RQ_SRILA,NOQ        NO                                           
         JE    *+12                                                             
         CLI   RQ_SRILA,C' '       BLANK MEANS NO                               
         JH    TSTSET12                                                         
         TM    ACTRSTAT,ACTSLOCK   IF NO OR BLANK THEN SKIP LOCKED              
         JNZ   EXITN                                                            
*                                                                               
TSTSET12 CLI   RQ_SRYNO,C'O'       ONLY                                         
         JNE   TSTSET14                                                         
         TM    ACTRSTAT,ACTSDRFT   ONLY LOCKED ACCOUNTS                         
         JZ    EXITN                                                            
         J     TSTSET16                                                         
*                                                                               
TSTSET14 CLI   RQ_SRYNO,C'N'                                                    
         JE    *+12                                                             
         CLI   RQ_SRYNO,C' '       BLANK MEANS NO                               
         JNE   TSTSET16                                                         
*                                                                               
         TM    ACTRSTAT,ACTSDRFT                                                
         JNZ   EXITN                                                            
*                                                                               
TSTSET16 DS    0H                                                               
         CLI   XL#CLVL,0                                                        
         JNE   TSTSET18                                                         
         TM    ACTRSTAT,ACTSABLP                                                
         JZ    EXITN                                                            
TSTSET18 L     R1,AIO1                                                          
         CLI   RQ_SRLCK,YESQ                                                    
         JE    TSTSET19                                                         
         GOTOR PROSPEC                                                          
         JNE   EXITN                                                            
TSTSET19 LA    R1,ACTRFST                                                       
         XR    R0,R0                                                            
         LA    RE,ACTRFST                                                       
*&&UK                                                                           
         MVI   XL#MYSW,NOQ                                                      
         CLI   RQ_SRVIO,C' '                                                    
         JH    TSTSET20                                                         
         MVI   XL#MYSW,YESQ                                                     
*&&                                                                             
TSTSET20 XR    R0,R0                                                            
         USING NAMELD,RE                                                        
TSTSET22 CLI   NAMEL,0                                                          
         JE    TSTSET32                                                         
         TM    CPYSTATC,CPYSMEDN                                                
         JZ    TSTSET24                                                         
         CLI   NAMEL,SNMELQ                                                     
         JE    TSTSET28                                                         
TSTSET24 TM    CPYSTATC,CPYSALTN                                                
         JZ    TSTSET26                                                         
         CLC   RQ_SRCUL,=C'1R'                                                  
         JE    TSTSET26                                                         
         CLI   NAMEL,XNMELQ                                                     
         JE    TSTSET30                                                         
TSTSET26 IC    R0,NAMLN                                                         
         AR    RE,R0                                                            
         J     TSTSET22                                                         
*                                                                               
         USING SNMELD,RE                                                        
TSTSET28 XR    R1,R1                                                            
         MVI   BYTE3,1             set found alternative name                   
         IC    R1,SNMLN                                                         
         SHI   R1,SNMLN1Q+1                                                     
         BASR  RF,0                                                             
         MVC   TEMP2(0),SNMNAME                                                 
         EX    R1,0(RF)                                                         
         J     TSTSET26                                                         
*                                                                               
         USING XNMELD,RE                                                        
TSTSET30 XR    R1,R1                                                            
         MVI   BYTE3,1             set found alternative name                   
         IC    R1,XNMSUBL                                                       
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   TEMP2(0),XNMSUBN                                                 
         EX    R1,0(RF)                                                         
         J     TSTSET26                                                         
         DROP  RE                                                               
*                                                                               
         USING NAMELD,RE                                                        
TSTSET32 LA    RE,ACTRFST                                                       
TSTSET34 CLI   NAMEL,0                                                          
         JE    TSTSET48                                                         
         CLI   NAMEL,NAMELQ                                                     
         JE    TSTSET38                                                         
         CLI   NAMEL,RSTELQ                                                     
         JE    TSTSET40                                                         
         CLI   NAMEL,EMPELQ                                                     
         JE    TSTSET46                                                         
*&&UK*&& CLI   NAMEL,RATEVATQ                                                   
*&&UK*&& JE    TSTSET42                                                         
         CLI   NAMEL,RATEDSCQ                                                   
         JE    TSTSET43                                                         
         CLI   NAMEL,PPRELQ                                                     
         JE    TSTSET45                                                         
TSTSET36 IC    R0,NAMLN                                                         
         AR    RE,R0                                                            
         J     TSTSET34                                                         
TSTSET38 CLI   BYTE3,1             did we find alternative name                 
         JE    TSTSET36            yes                                          
         XR    R1,R1               no - get normal name                         
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         BASR  RF,0                                                             
         MVC   TEMP2(0),NAMEREC                                                 
         EX    R1,0(RF)                                                         
         J     TSTSET36                                                         
*                                                                               
         USING RSTELD,RE                                                        
TSTSET40 CLC   RSTSECY+1(1),CUAUTH+1                                            
         JH    EXITN                                                            
         CLI   RQ_SRVIO,C' '                                                    
         JNH   TSTSET36                                                         
         CLI   RQ_SRVIO,C'I'                                                    
         JE    TSTSET41                                                         
         TM    RSTSTAT1,RSTSIVAT                                                
         JNZ   EXITN                                                            
         J     TSTSET36                                                         
*                                                                               
TSTSET41 TM    RSTSTAT1,RSTSIVAT                                                
         JZ    EXITN                                                            
         J     TSTSET36                                                         
*                                                                               
         USING RATELD,RE                                                        
*&&UK                                                                           
TSTSET42 CLC   LDGAUL,SGQ                                                       
         JNE   TSTSET36                                                         
         MVC   HALF1,RATRATE       set rate                                     
         MVI   XL#MYSW,YESQ                                                     
         J     TSTSET36                                                         
*&&                                                                             
*                                                                               
TSTSET43 CLC   LDGAUL,SVQ                                                       
         JE    TSTSET44                                                         
         CLC   LDGAUL,SFQ                                                       
         JE    TSTSET44                                                         
         CLC   LDGAUL,STQ                                                       
         JNE   TSTSET36                                                         
TSTSET44 MVC   HALF2,RATRATE       set rate                                     
         J     TSTSET36                                                         
*                                                                               
         USING PPRELD,RE                                                        
TSTSET45 CLC   LDGAUL,PRODUL                                                    
         JNE   TSTSET36                                                         
         MVC   XL#AOFF,PPRGAOFF    save office                                  
         J     TSTSET36                                                         
*                                                                               
         USING EMPELD,RE                                                        
TSTSET46 CLC   LDGAUL,=C'1R'       this check is on 1R only                     
         JNE   TSTSET36                                                         
         CLI   XL#APPL,RQ_SREST    For estimates we only want                   
         JNE   TSTSET36                        active people                    
         CLI   EMPCSTAT,0                                                       
         JE    TSTSET36                                                         
*        CLI   EMPCSTAT,EMPCTRM                                                 
*        JE    TSTSET36                                                         
         J     EXITN                                                            
*                                                                               
TSTSET48 DS    0H                                                               
*&&UK*&& CLI   XL#MYSW,YESQ        vat rate found?                              
*&&UK*&& JNE   EXITN                                                            
         OC    XL#AOFF,SPACES                                                   
*&&UK*&& TM    CPXSTATA,CPXLACAC                                                
*&&UK*&& JNZ   *+14                                                             
         CLC   XL#AOFF,SPACES      OK if no office set                          
         JE    EXITY                                                            
*&&UK*&& OC    XL#COFF,XL#COFF                                                  
*&&UK*&& JZ    TSTSET50                                                         
*&&US*&& CLC   XL#COFF,SPACES                                                   
*&&US*&& JNH   TSTSET50                                                         
         CLC   XL#COFF,XL#AOFF     If not equal to filter office                
         JNE   EXITN                                                            
         USING OFFALD,R1                                                        
TSTSET50 CLI   CUACCS,0            LIMIT ACCESS?                                
         JE    EXITY                                                            
         L     R1,AOFFAREA                                                      
*&&UK                                                                           
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    TSTSEC52                                                         
         TM    CPXSTATA,CPXLACAC   Test limit account access in use             
         JZ    TSTSEC52                                                         
         MVC   OFFAOPOS,LDGAOP                                                  
         ST    R3,OFFAREC                                                       
         MVI   OFFAACT,OFFATST                                                  
         J     TSTSEC54                                                         
*&&                                                                             
TSTSEC52 MVC   OFFAOFFC,XL#AOFF                                                 
         MVI   OFFAACT,OFFAVAL                                                  
TSTSEC54 GOTO1 VOFFAL              VALIDATE OFFICE                              
         JE    EXITY                                                            
         LA    R4,ACTRFST-ACTRECD(R3)                                           
         GOTOR CHKOFF                                                           
         JNE   EXITN                                                            
         J     EXIT                                                             
         DROP  R3,RE                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* Filter account against limit list                                   *         
* Entry: LIACBLK - account code limit list                            *         
*        R1      - account code                                       *         
***********************************************************************         
                                                                                
FLTACT   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*FLTACT*'                                                      
*                                                                               
         STC   R1,XL#TYPE                                                       
         MVC   XL#ULA,WORK                                                      
         CLI   XL#APPL,RQ_MAINT                                                 
         JE    EXITY                                                            
*                                                                               
         LAY   R4,LIACBLK                                                       
         OC    0(L'ACTKACT,R4),0(R4)                                            
         JZ    FLTACT16              No - check media code list                 
*                                                                               
FLTACT02 OC    0(L'ACTKACT,R4),0(R4)                                            
         JZ    EXITN               End of list - get next                       
                                                                                
         CLC   =C'SX',XL#UL        Creditor ledger ?                            
         JE    FLTACT04                                                         
         CLC   =C'SV',XL#UL                                                     
         JE    FLTACT04                                                         
         CLC   =C'SF',XL#UL                                                     
         JE    FLTACT04                                                         
         CLC   =C'ST',XL#UL                                                     
         JNE   *+18                                                             
FLTACT04 CLC   XL#UL,0(R4)                                                      
         JNE   FLTACT12                                                         
         LA    R4,L'XL#UL(R4)                                                   
         CLI   0(R4),C' '                                                       
         JE    FLTACT20                                                         
                                                                                
FLTACT06 SR    R1,R1               Find shortest account length                 
         IC    R1,LDGAL1           Get length of level A                        
         CHI   R1,L'ACTKACT        Single level ledger?                         
         JE    FLTACT10            Yes - OK                                     
                                                                                
         LA    RF,XL#ACT(R1)         1st character of level B                   
         CLI   0(RF),C' '                                                       
         JE    FLTACT10            Level B is blank this is level A             
         LA    RF,0(R1,R4)                                                      
         CLI   0(RF),C' '          Check limitlist account code                 
         JE    FLTACT10                                                         
*                                                                               
         IC    R1,LDGAL2           Get length of level B                        
         CHI   R1,L'ACTKACT        2 level account?                             
         JE    FLTACT10            Yes - OK                                     
         LA    RF,XL#ACT(R1)       1st character of level C                     
         CLI   0(RF),C' '                                                       
         JE    FLTACT10            Level C is blank this is level B             
         LA    RF,0(R1,R4)                                                      
         CLI   0(RF),C' '          Check limitlist account code                 
         JE    FLTACT10                                                         
*                                                                               
         IC    R1,LDGAL3           Get length of level C                        
         CHI   R1,L'ACTKACT        3 level account?                             
         JE    FLTACT10            Yes - OK                                     
         LA    RF,XL#ACT(R1)       1st character of level D                     
         CLI   0(RF),C' '                                                       
         JE    FLTACT10            Level D is blank this is level C             
         LA    RF,0(R1,R4)                                                      
         CLI   0(RF),C' '          Check limitlist account code                 
         JE    FLTACT10                                                         
         IC    R1,LDGAL4           This is level D                              
*                                                                               
FLTACT10 SHI   R1,1                                                             
         BASR  RF,0                                                             
         CLC   XL#ACT(0),0(R4)                                                  
         EX    R1,0(RF)                                                         
         JE    FLTACT15            Check media code limit list                  
                                                                                
FLTACT12 LA    R4,L'ACTKACT(,R4)                                                
         CLC   PRODUL,XL#UL        SJ ledger ?                                  
         JE    FLTACT14            Yes - need to add office                     
         CLC   =C'SX',XL#UL        Creditor ledger ?                            
         JE    FLTACT14            Yes - need to add unit ledger                
         CLC   =C'SV',XL#UL                                                     
         JE    FLTACT14                                                         
         CLC   =C'SF',XL#UL                                                     
         JE    FLTACT14                                                         
         CLC   =C'ST',XL#UL                                                     
         JNE   FLTACT02                                                         
FLTACT14 LA    R4,L'LIDLOFF(R4)                                                 
         J     FLTACT02            Try next in the cli/pro/job list             
                                                                                
FLTACT15 CLC   PRODUL,XL#UL        SJ ledger ?                                  
         JNE   FLTACT20                                                         
         CLI   XL#CLVL,1           client level                                 
         JE    FLTACT20            Ignore office                                
                                                                                
         LA    R4,L'ACTKACT(R4)                                                 
         CLC   0(L'TRNOFFC,R4),SPACES  Do we have an office                     
         JNH   FLTACT20            No - ignore office check                     
         CLC   WORK+L'ACTKULA(L'XL#COFF),SPACES Any offices?                    
         JNH   FLTACT20                                                         
         CLC   WORK+L'ACTKULA(L'XL#COFF),0(R4)                                  
         JNE   FLTACT14                                                         
         J     FLTACT20                                                         
*                                                                               
FLTACT16 CLC   PRODUL,XL#UL        SJ ledger ?                                  
         JNE   FLTACT30                                                         
         L     R1,AIO8                                                          
FLTACT18 OC    0(L'XL#COFF,R1),0(R1) Any office list                            
         JZ    FLTACT30            No - we want this                            
         CLC   WORK+L'ACTKACT(L'XL#COFF),0(R1)                                  
         JE    FLTACT20                                                         
         LA    R1,L'XL#COFF(R1)    Try next office                              
         J     FLTACT18                                                         
*                                                                               
FLTACT20 CLC   PRODUL,XL#UL        SJ ledger ?                                  
         JNE   EXITY                                                            
         XR    R1,R1                                                            
         IC    R1,LDGAL2           Product length                               
         LA    RF,XL#ACT(R1)       First character of job code                  
         CLI   0(RF),C' '          Any media code?                              
         JNH   EXITY               No - ok                                      
         L     R1,AIO7                                                          
         OC    0(L'ELEMENT,R1),0(R1) Any meida code list                        
         JNZ   FLTACT30            Yes                                          
         CLI   X#LLTMED,X#LLALLQ   Do we have access to all media codes         
         JE    EXITY               Yes                                          
         J     EXITN               No                                           
*                                                                               
FLTACT30 CLC   PRODUL,XL#UL        SJ ledger ?                                  
         JNE   FLTACT50                                                         
         L     R1,AIO7                                                          
         OC    0(L'ELEMENT,R1),0(R1) Any meida code list                        
         JZ    FLTACT50            No - we want this                            
         XR    R1,R1                                                            
         IC    R1,LDGAL2           Product length                               
         LA    RF,XL#ACT(R1)       First character of job code                  
         CLI   0(RF),C' '          Any media code?                              
         JNH   EXITY               No - ok                                      
*                                                                               
         L     R1,AIO7             Media code list                              
FLTACT40 CLI   0(R1),C' '                                                       
         JNH   EXITN               End of media code list                       
         CLC   0(1,R1),0(RF)                                                    
         JE    EXITY                                                            
         LA    R1,1(,R1)           Try next in the media code list              
         J     FLTACT40                                                         
*                                                                               
FLTACT50 CLI   XL#APPR,YESQ        are we checking approver list                
         JNE   FLTACT70            no                                           
         CLI   XL#USAP,YESQ        is user an approver                          
         JE    FLTACT60            yes filter accounts accordingly              
         CLI   XL#TYPE,XL#SRCH     Are we searching or listing                  
         JE    FLTACT52            Searching                                    
         CLI   XL#ALL,YESQ         Are we doing a 1R all list                   
         JE    EXITN                                                            
FLTACT52 CLI   XL#LIML,YESQ        no - therefore nothing to give back          
         JE    EXITY                        unless we have override on          
         J     EXITN                                                            
                                                                                
FLTACT60 GOTOR ADVAPP                                                           
         JE    EXITY                                                            
* The following two lines were reinstated for BR39021L (SMAN 18Jan11)           
                                                                                
         CLI   XL#TYPE,XL#SRCH     Are we searching or listing                  
         JE    FLTACT62            Searching                                    
         CLI   XL#ALL,YESQ         Are we doing a 1R all list                   
         JE    EXITN                                                            
FLTACT62 CLI   XL#LIML,YESQ        Do we have override on                       
         JE    EXITY               Yes - allow record                           
         J     EXITN                                                            
                                                                                
FLTACT70 CLC   PRODUL,XL#UL                                                     
         JNE   FLTACT72                                                         
         CLI   X#LLTCPJ,X#LLALLQ                                                
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
FLTACT72 CLC   =C'1R',XL#UL                                                     
         JNE   FLTACT74                                                         
         CLI   X#LLT1R,X#LLALLQ                                                 
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
FLTACT74 CLC   =C'1N',XL#UL                                                     
         JNE   FLTACT76                                                         
         CLI   X#LLTNC,X#LLALLQ                                                 
         JNE   EXITN                                                            
         J     EXITY                                                            
                                                                                
FLTACT76 CLI   XL#APPL,RQ_SRINV    Invoices only                                
         JNE   EXITY                                                            
         CLC   =C'SX',XL#UL        Creditor ledger ?                            
         JE    FLTACT78                                                         
         CLC   =C'SV',XL#UL                                                     
         JE    FLTACT78                                                         
         CLC   =C'SF',XL#UL                                                     
         JE    FLTACT78                                                         
         CLC   =C'ST',XL#UL                                                     
         JNE   EXITY                                                            
FLTACT78 CLI   X#LLTSUP,X#LLALLQ                                                
         JNE   EXITN                                                            
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
ACCRUL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*ACCRUL*'                                                      
*                                                                               
         MVI   X#LLTABS,X#LLALLQ   Default to unlimited access                  
         MVC   X#LLTABS+1(X#LLTLQ-1),X#LLTABS                                   
                                                                                
         LA    RF,SCPXEL                                                        
         GOTOR SETLAV              Set limit access values at company           
         GOTOR CHKLAV                                                           
         JE    ACCRULX             All values now set                           
                                                                                
         CLI   CUACCS,C'$'         List access single character                 
         JE    ACCRUL40            Yes - don't read for offices                 
         USING OFLPASD,R2                                                       
         LA    R2,IOKEY                                                         
         XC    OFLPAS,OFLPAS                                                    
         MVI   OFLPTYP,OFLPTYPQ                                                 
         MVI   OFLPSUB,OFLPSUBQ                                                 
         MVC   OFLPREM,CUXCPY                                                   
         MVC   OFLPOFF,CUACCS+1                                                 
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+10                                                             
         MVC   OFLPOFF,CUACCS+2                                                 
         MVC   CSVKEY1,OFLPAS                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    ACCRUL20                                                         
         J     ACCRUL40                                                         
                                                                                
ACCRUL10 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO3'                               
         JNE   ACCRUL40                                                         
                                                                                
ACCRUL20 CLC   OFLPAS(OFLPOFL-OFLPASD),CSVKEY1                                  
         JNE   ACCRUL40                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R0,R0                                                            
         L     RF,AIO3                                                          
         LA    RF,OFFRFST-OFFRECD(RF)                                           
N        USING CPXELD,RF                                                        
ACCRUL30 CLI   N.CPXEL,0                                                        
         JE    ACCRUL10                                                         
         CLI   N.CPXEL,CPXELQ                                                   
         JE    *+14                                                             
         IC    R0,N.CPXLN                                                       
         AR    RF,R0                                                            
         J     ACCRUL30                                                         
         DROP  N                                                                
*                                                                               
         GOTOR SETLAV              SET LIMIT ACCESS VALUES AT OFL LVL           
         J     ACCRUL10                                                         
                                                                                
ACCRUL40 GOTOR CHKLAV                                                           
         JE    ACCRULX             OK - ALL VALUES NOW SET                      
                                                                                
         LA    R2,IOKEY            ELSE TRY OFFICE FOR SETTINGS                 
         USING OFFRECD,R2                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUXCPY                                                   
         MVC   OFFKOFF,CUACCS+1    *** THIS PROBABLY WON'T DO...                
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+10                                                             
         MVC   OFFKOFF,CUACCS+2                                                 
         MVC   CSVKEY1,OFFKEY                                                   
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO3'                               
         JNE   ACCRULX             NO OFFICE RECORD                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R0,R0                                                            
         L     RF,AIO3                                                          
         LA    RF,OFFRFST-OFFRECD(RF)                                           
N        USING CPXELD,RF                                                        
ACCRUL50 CLI   N.CPXEL,0                                                        
         JE    ACCRULX                                                          
         CLI   N.CPXEL,CPXELQ                                                   
         JE    *+14                                                             
         IC    R0,N.CPXLN                                                       
         AR    RF,R0                                                            
         J     ACCRUL50                                                         
         DROP  N                                                                
                                                                                
         GOTOR SETLAV            SET LIMIT ACCESS VALUES AT OFF LVL             
         DROP  R2                                                               
                                                                                
ACCRULX  J     EXIT                                                             
                                                                                
         EJECT                                                                  
                                                                                
                                                                                
N        USING CPXELD,RF                                                        
SETLAV   CLI   X#LLTMED,X#LLALLQ   TEST 'ALL' (= DEFAULT)                       
         JNE   *+16                                                             
         TM    N.CPXSTAT3,CPXAMED                                               
         JZ    *+8                                                              
         MVI   X#LLTMED,X#LLNONQ                                                
                                                                                
         CLI   X#LLTCPJ,X#LLALLQ                                                
         JNE   *+16                                                             
         TM    N.CPXSTAT3,CPXAJOBS                                              
         JZ    *+8                                                              
         MVI   X#LLTCPJ,X#LLNONQ                                                
                                                                                
         CLI   X#LLTWC,X#LLALLQ                                                 
         JNE   *+16                                                             
         TM    N.CPXSTAT3,CPXAWC                                                
         JZ    *+8                                                              
         MVI   X#LLTWC,X#LLNONQ                                                 
                                                                                
         CLI   X#LLTETY,X#LLALLQ                                                
         JNE   *+16                                                             
         TM    N.CPXSTAT3,CPXAETYP                                              
         JZ    *+8                                                              
         MVI   X#LLTETY,X#LLNONQ                                                
                                                                                
         CLI   X#LLTSUP,X#LLALLQ                                                
         JNE   *+16                                                             
         TM    N.CPXSTAT4,CPXASUPP                                              
         JZ    *+8                                                              
         MVI   X#LLTSUP,X#LLNONQ                                                
                                                                                
         CLI   X#LLT1R,X#LLALLQ                                                 
         JNE   *+16                                                             
         TM    N.CPXSTAT3,CPXASTAF                                              
         JZ    *+8                                                              
         MVI   X#LLT1R,X#LLNONQ                                                 
                                                                                
         CLI   X#LLTNC,X#LLALLQ                                                 
         JNE   *+16                                                             
         TM    N.CPXSTAT3,CPXA1NAC                                              
         JZ    *+8                                                              
         MVI   X#LLTNC,X#LLNONQ                                                 
         BR    RE                                                               
         DROP  N                                                                
                                                                                
* TEST ALL LIMIT ACCESS VALUES ARE SET                                          
                                                                                
CHKLAV   LA    RF,X#LLTABS                                                      
         LHI   R0,X#LLTLQ                                                       
         CLI   0(RF),X#LLALLQ      'ALL' = DEFAULT                              
         JE    CHKLAVN             SO TRY ANOTHER LEVEL                         
         LA    RF,1(RF)                                                         
         JCT   R0,*-12                                                          
CHKLAVY  CR    RB,RB                                                            
         BR    RE                                                               
CHKLAVN  LTR   RB,RB                                                            
         BR    RE                                                               
                                                                                
                                                                                
***********************************************************************         
* GET DUE DATE                                                        *         
***********************************************************************         
GETDDF   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    C'*GETDDF*'                                                      
*                                                                               
         LM    R2,R3,0(R1)                                                      
         USING DEXELD,R2                                                        
         LA    RF,WORK                                                          
         USING CONBLKD,RF                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONATRAQ    TRANSLATE                                    
         MVI   CONFLD,CONFIDUE     DUE DATE EXPRESSION                          
         MVI   CONILEN,L'DEXVAL    LENGTH OF CODE                               
         LA    RE,DEXVAL                                                        
         STCM  RE,15,CONIADD       INPUT ADDRESS                                
         LA    RE,0(R3)                                                         
         STCM  RE,15,CONOADD       OUTPUT ADDRESS                               
         MVC   CONCOMF,ACOMFACS    A(COMFACS)                                   
         GOTOR VCONVERT,CONBLK                                                  
         J     EXITY                                                            
         DROP  R2                                                               
                                                                                
                                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ACCOUNT NAME SEARCH (VARIABLE)                                      *         
* - clone from FlexiBill ACCLB41/6E new search                        *         
***********************************************************************         
XSEARCH  NTR1  LABEL=NO,WORK=(RC,SRWORKL)                                       
         J     *+12                                                             
         DC    C'*XSEARCH'                                                      
                                                                                
         USING SRWORKD,RC                                                       
         GOTOR CLRWRK,SRWORKL      Clear work area                              
         USING OB_D,SRRBAREA                                                    
*                                                                               
         GOTOR INILBUF             INITIALISE LOCAL BUFFER                      
         LAY   R0,GENAREA          Clear genarea as used by getopt              
         LHI   R1,GENAREAX-GENAREA                                              
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVI   XL#GLIND,0                                                       
         GOTOR ACCRUL                                                           
*        L     R4,AGENAREA                                                      
*        MVI   0(R4),X'FF'                                                      
*                                                                               
SEARCH00 MVC   XL#APPL,RQ_SRAPL                                                 
         CLI   XL#APPL,RQ_ADEXP    For time and expenses read approver          
         JE    *+12                if no limit list present                     
         CLI   XL#APPL,RQ_ADTIM                                                 
         JNE   SEARCH02                                                         
         CLC   RQ_SRCUL,=C'1R'                                                  
         JNE   SEARCH02                                                         
         MVI   XL#APPR,C'Y'        yes - set to use approval list               
SEARCH02 MVC   XL#LIML,RQ_SRCSF                                                 
         MVI   XL#USAP,NOQ         No default is not an approver                
*                                                                               
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         XC    XL#CWNO,XL#CWNO                                                  
         XC    XL#SRAL(5),XL#SRAL                                               
         CLC   RQ_SRCUL,=C'2P'     IF DOING 2P READ 2D LEDGER FIRST             
         JNE   SEARCH03                                                         
         MVC   LDGAUL,=C'2D'                                                    
         J     *+10                                                             
*                                                                               
SEARCH03 MVC   LDGAUL,RQ_SRCUL                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         JNE   SEARC210            (empty if error)                             
         CLI   LDGAOP,LDGOTRAN                                                  
         JNE   *+8                                                              
         MVI   LDGAOP,LDGONONE                                                  
         CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
         CLC   LDGAUL,=C'2D'                                                    
         JNE   *+16               IF DOING 2D SEARCH SAVE POS AND LEN           
         MVC   SAVEDPOS,LDGAOP    OF OFFICE                                     
         MVC   SAVEDLEN,LDGAL1                                                  
         CLC   RQ_SRCUL,=C'2P'    IF DOING 2D SEARCH DON'T NEED TO READ         
         JNE   SEARCH05           ELEMS                                         
         XC    LDGAREA(LDGALNQ),LDGAREA                                         
         MVC   LDGAUL,RQ_SRCUL                                                  
         GOTOR (#SETLDG,ASETLDG)                                                
         JNE   SEARC210                                                         
         CLI   LDGAOP,LDGOTRAN                                                  
         JNE   *+8                                                              
         MVI   LDGAOP,LDGONONE                                                  
         CLI   LDGAOP,LDGONKHI                                                  
         JH    *+8                                                              
         NI    LDGAOP,FF-LDGOKEY2                                               
*                                                                               
SEARCH3B L     R1,AIO1                                                          
         XR    R0,R0                                                            
         USING LDGRECD,R1                                                       
         LA    R1,LDGRFST                                                       
*                                                                               
         USING LDGELD,R1                                                        
SEARCH04 CLI   LDGEL,0             GET DEP. POS/LEN                             
         JNE   *+6                                                              
         DC    H'0'                ERROR NO LEDGER ELEMENT                      
         CLI   LDGEL,LDGELQ                                                     
         JE    SEARCH05                                                         
         IC    R0,LDGLN                                                         
         AR    R1,R0                                                            
         J     SEARCH04            BUMP TO NEXT ELEM                            
*                                                                               
SEARCH05 CLC   RQ_SRCUL,=C'2D'     CHECK WHETHER 2D                             
         JE    *+14                                                             
         CLC   RQ_SRCUL,=C'2P'     CHECK WHETHER 2P                             
         JNE   SEARCH07                                                         
         CLC   RQ_SRCAC,SPACES     ACCOUNT PASSED?                              
         JH    SEARCH07            THEN DON'T BOTHER                            
         CLI   LDGAOP,1            FOR 2D SEARCH ONLY                           
         JNE   SEARCH06                                                         
         LLC   RF,LDGAL1                                                        
         SHI   RF,1                                                             
         BASR  RE,0                                                             
         MVC   RQ_SRCAC(0),RQ_SRCOF PUT IN OFFICE                               
         EX    RF,0(RE)                                                         
         CLC   RQ_SRCUL,=C'2P'     IF 2D OK TO EXIT                             
         JNE   SEARCH07                                                         
*                                                                               
SEARCH06 CLC   RQ_SRCUL,=C'2D'     IF 2D OK TO EXIT                             
         JE    SEARCH07                                                         
         LA    RE,RQ_SRCAC                                                      
         XR    R2,R2                                                            
         CLI   LDGDPOS,X'00'                                                    
         JE    SEARCH07                                                         
         LLC   RF,LDGDPOS                                                       
         SHI   RF,1                subtract 1 to get correct position           
         AR    RE,RF                                                            
         CLI   LDGDLEN,X'00'                                                    
         JE    SEARCH07                                                         
         IC    RF,LDGDLEN                                                       
         SHI   RF,1                                                             
         LA    R2,RQ_SRDEP                                                      
         CLI   SAVEDPOS,1          CHECK WHETHER WE HAD OFFICE POSITION         
         JNE   *+12                NO OK TO MOVE IN DEPARTMENT AS IS            
         IC    R0,SAVEDLEN         BUMP ALONG TO STRIP OFF OFFICE               
         AR    R2,R0                                                            
         DROP  R1                                                               
         BASR  R1,0                                                             
         MVC   0(0,RE),0(R2)       MOVE IN DEPARTMENT                           
         EX    RF,0(R1)                                                         
*                                                                               
SEARCH07 DS    0H                                                               
*&&UK*&& GOTOR UMLAUT                                                           
         GOTOR VALAPP,RQ_SRCUL     is this person an approver for this          
         JNE   SEARCH08                                      ledger             
         MVI   XL#USAP,YESQ        Yes                                          
SEARCH08 XC    XL#COFF,XL#COFF                                                  
         CLC   RQ_SRCOF,SPACES     office filter?                               
         JNH   SEARCH12                                                         
         MVC   XL#COFF,RQ_SRCOF                                                 
         OC    XL#COFF,SPACES                                                   
         CLI   LDGAOP,0            office in key is OK                          
         JE    SEARCH10                                                         
         CLI   LDGAOP,LDGOKEY                                                   
         JNH   SEARCH12                                                         
         CLI   LDGAOP,LDGOPROF     profile (SJ) is OK, too                      
         JE    SEARCH12                                                         
SEARCH10 XC    XL#COFF,XL#COFF     office filter cannot JE applied              
SEARCH12 MVC   XL#CLVL,RQ_SRCLV    set level requested                          
         NI    XL#CLVL,X'0F'                                                    
         MVI   XL#SRAL,FF          any account limitation?                      
*&&US*&& OC    RQ_SRCAC,SPACES     always use upper case                        
         LA    RE,RQ_SRCAC+L'RQ_SRCAC-1                                         
         LA    R1,L'RQ_SRCAC                                                    
SEARCH14 CLI   0(RE),C' '                                                       
         JH    SEARCH16                                                         
         SHI   RE,1                                                             
         JCT   R1,SEARCH14                                                      
         J     SEARCH18                                                         
SEARCH16 SHI   R1,1                                                             
         STC   R1,XL#SRAL          set executable length of account             
         MVC   TEMP2,SPACES                                                     
         BASR  RE,0                                                             
         MVC   TEMP2(0),RQ_SRCAC                                                
         EX    R1,0(RE)                                                         
*                                                                               
SEARCH18 DS    0H                                                               
*&&US                                                                           
         CLC   RQ_SRCW1,SPACES     Anything in the search field 1?              
         JNH   SEARCH28                No - skip                                
         CLC   RQ_SRCW2,SPACES     Anything in the search field 2?              
         JH    SEARCH28                Yes - skip (problem fixed)               
         CLC   RQ_SRCW3,SPACES     Anything in the search field 3?              
         JH    SEARCH28                Yes - skip (problem fixed)               
*                                                                               
         MVC   WORK(L'RQ_SRCW1),RQ_SRCW1  save off search word(s)               
         MVI   BYTE1,0                    save off search status                
         XR    R1,R1                      Init field counter                    
         LA    R0,L'RQ_SRCW1              Check if multiple words               
         LA    R2,WORK                    always keep R2=WORK fld start         
         LR    RE,R2                                                            
         LA    RF,SRCHTAB          point to table of search fields              
SEARCH20 CLI   0(RE),C' '          Are we at a space?                           
         JH    SEARCH24                                                         
         ST    RE,FULL1                                                         
         IC    R1,BYTE1                                                         
         AHI   R1,-1                                                            
         JNP   SEARCH22            Two spaces in a row?                         
*                                                                               
         CLI   0(RF),EOF           Are we done?                                 
         JE    SEARCH28                                                         
         XR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         LA    RE,RQ_DATA(RE)                                                   
         MVC   0(L'RQ_SRCW1,RE),SPACES                                          
         BASR  R4,0                                                             
         MVC   0(0,RE),0(R2)       Move in the first field                      
         EX    R1,0(R4)                                                         
         XR    RE,RE                                                            
         IC    RE,1(RF)            Get Type                                     
         LA    RE,RQ_DATA(RE)                                                   
         MVC   0(L'RQ_SRCS1,RE),RQ_SRCS1                                        
         LA    RF,2(RF)            Bump to next search field in SRCHTAB         
*                                                                               
SEARCH22 LA    RE,WORK                                                          
         LA    R1,L'RQ_SRCW1(RE)   Get to end of Search field                   
         SR    R1,R2               get length of remaining data                 
         AHI   R1,-1                                                            
         JNP   SEARCH28                                                         
         BASR  RE,0                                                             
         CLC   0(0,R2),SPACES      Anything left?                               
         EX    R1,0(RE)                                                         
         JNH   SEARCH28                                                         
*                                                                               
         L     RE,FULL1                                                         
         LA    R2,1(RE)            Re-address R2 to search field                
         MVI   BYTE1,0             Re-init length field                         
         J     SEARCH26                                                         
*                                                                               
SEARCH24 IC    R1,BYTE1                                                         
         AHI   R1,1                                                             
         STC   R1,BYTE1                                                         
SEARCH26 LA    RE,1(RE)                                                         
         JCT   R0,SEARCH20                                                      
*                                                                               
SEARCH28 DS    0H                                                               
*&&                                                                             
         MVI   XL#SRWN,1           set # of words to JE searched for            
         LA    RE,RQ_SRCW1         determine exec length of word 1              
         XR    RF,RF                                                            
         LA    R1,L'RQ_SRCW1                                                    
SEARCH30 CLI   0(RE),C' '                                                       
         JNH   SEARCH32                                                         
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         JCT   R1,SEARCH30                                                      
SEARCH32 STC   RF,XL#SRW1                                                       
         CLC   RQ_SRCW2,SPACES                                                  
         JNH   SEARCH42                                                         
         MVI   XL#SRWN,2                                                        
         LA    RE,RQ_SRCW2         determine exec length of word 2              
         XR    RF,RF                                                            
         LA    R1,L'RQ_SRCW2                                                    
SEARCH34 CLI   0(RE),C' '                                                       
         JNH   SEARCH36                                                         
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         JCT   R1,SEARCH34                                                      
*        DC    H'0'                                                             
                                                                                
SEARCH36 STC   RF,XL#SRW2                                                       
         CLC   RQ_SRCW3,SPACES                                                  
         JNH   SEARCH42                                                         
         MVI   XL#SRWN,3                                                        
         LA    RE,RQ_SRCW3         determine exec length of word 3              
         XR    RF,RF                                                            
         LA    R1,L'RQ_SRCW3                                                    
SEARCH38 CLI   0(RE),C' '                                                       
         JNH   SEARCH40                                                         
         AHI   RE,1                                                             
         AHI   RF,1                                                             
         JCT   R1,SEARCH38                                                      
*        DC    H'0'                                                             
                                                                                
SEARCH40 STC   RF,XL#SRW3                                                       
*                                                                               
SEARCH42 MVC   AELEPTRR,AGENAREA   initialise table (use GENAEXTN)              
         XC    XL#SRCT,XL#SRCT     and set count to 0                           
         MVI   XL#LIND,0                                                        
*                                                                               
         XC    ELEMENT,ELEMENT     Clear media code list                        
         LAY   RE,LIACBLK          Clear cli/pro/job list                       
         ST    RE,XL#ADR1                                                       
         LHI   RF,L'LIACBLK                                                     
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         MVCL  RE,R0                                                            
                                                                                
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO7                                        
         GOTOR (#CLRIO,ACLRIO),DMCB,AIO8                                        
                                                                                
         OC    CCTPID,CCTPID                                                    
         JZ    SEARCH66                                                         
         CLC   RQ_SRCUL,PRODUL     SJ only                                      
         JNE   SEARCH58                                                         
                                                                                
SEARCH44 CLI   RQ_SRCSF,YESQ       Security override?                           
         JE    SEARCH48                                                         
*                                                                               
         CLI   XL#CLVL,3           Job list request                             
         JNE   SEARCH46                                                         
         GOTOR GETLMT,'LIDTMEDL'   get media code limit list                    
SEARCH46 GOTOR GETLMT,'LIDTCPJL'   get cli/pro/job limit list                   
         CLI   XL#CLVL,3           Job list request                             
         JNE   SEARCH48                                                         
         CLI   X#LLTCPJ,X#LLNONQ   if SJ or media is empty exit here            
         JE    SEARC210                                                         
         CLI   X#LLTMED,X#LLNONQ                                                
         JE    SEARC210                                                         
SEARCH48 CLI   XL#CLVL,1           client list request                          
         JE    SEARCH66                                                         
         CLC   RQ_SRCAC,SPACES                                                  
         JNH   SEARCH66                                                         
         USING ACTRECD,R2          read Client (+Product) for office            
         LA    R2,IOKEY                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUXCPY                                                   
         MVC   ACTKUNT(L'PRODUL),PRODUL                                         
         SR    RE,RE                                                            
         IC    RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   ACTKACT(0),RQ_SRCAC                                              
         EX    RE,0(RF)                                                         
         MVC   CSVKEY2,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SEARC210                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SEARC210                                                         
*                                                                               
         L     R2,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R2)                                           
         USING PPRELD,R1                                                        
         SR    R0,R0                                                            
SEARCH50 CLI   PPREL,0                                                          
         JE    SEARCH52                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    *+14                                                             
         IC    R0,PPRLN                                                         
         AR    R1,R0                                                            
         J     SEARCH50                                                         
         MVC   XL#HOFF,PPRGAOFF                                                 
         OC    XL#HOFF,SPACES                                                   
*                                                                               
SEARCH52 CLI   XL#CLVL,3           Job list request                             
         JNE   SEARCH66                                                         
         LA    R2,IOKEY                                                         
         XR    RE,RE                                                            
         IC    RE,LDGAL2                                                        
         SHI   RE,1                                                             
         MVC   ACTKACT(0),RQ_SRCAC                                              
         EX    RE,0(RF)                                                         
         MVC   CSVKEY2,IOKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JNE   SEARC210                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SEARC210                                                         
*                                                                               
         L     R2,AIO1                                                          
         LA    R1,ACTRFST-ACTRECD(R2)                                           
         USING PPRELD,R1                                                        
         SR    R0,R0                                                            
SEARCH54 CLI   PPREL,0                                                          
         JE    SEARCH56                                                         
         CLI   PPREL,PPRELQ                                                     
         JE    *+14                                                             
         IC    R0,PPRLN                                                         
         AR    R1,R0                                                            
         J     SEARCH54                                                         
*                                                                               
SEARCH56 CLC   PPRGAOFF,SPACES                                                  
         JNH   SEARCH66                                                         
         MVC   XL#HOFF,PPRGAOFF                                                 
         OC    XL#HOFF,SPACES                                                   
         J     SEARCH66                                                         
*                                                                               
SEARCH58 CLI   RQ_SRCSF,YESQ       Security override?                           
         JE    SEARCH66                                                         
         CLC   RQ_SRCUL,=C'1R'     is it personnel account?                     
         JNE   SEARCH60                                                         
         GOTOR GETLMT,'LIDT1RAC'   get personnel limit list                     
         J     SEARCH66                                                         
*                                                                               
SEARCH60 CLC   RQ_SRCUL,=C'1N'     is it non client account ?                   
         JNE   SEARCH62                                                         
         GOTOR GETLMT,'LIDTNCLL'   get non client limit list                    
*                                                                               
SEARCH62 CLI   RQ_SRAPL,RQ_SRINV   is it supplier account ?                     
         JNE   SEARCH66                                                         
         CLC   RQ_SRCUL,=C'SX'                                                  
         JE    SEARCH64                                                         
         CLC   RQ_SRCUL,=C'SV'                                                  
         JE    SEARCH64                                                         
         CLC   RQ_SRCUL,=C'ST'                                                  
         JE    SEARCH64                                                         
         CLC   RQ_SRCUL,=C'SF'                                                  
         JNE   SEARCH66                                                         
                                                                                
SEARCH64 GOTOR GETLMT,'LIDTSUPP'   get non client limit list                    
*                                                                               
         USING SRCRECD,R2                                                       
SEARCH66 XR    R0,R0                                                            
         IC    R0,XL#LIND                                                       
         AHI   R0,1                                                             
         STC   R0,XL#LIND                                                       
         CHI   R0,3                                                             
         JH    SEARCH78            end of word list?                            
         LR    RF,R0                                                            
         SHI   RF,1                                                             
         MHI   RF,RQ_SRCW2-RQ_SRCW1                                             
         LA    RF,RQ_SRCW1(RF)                                                  
         CLC   0(L'RQ_SRCW1,RF),SPACES                                          
         JNH   SEARCH78            end of word list?                            
*                                                                               
         LA    R2,IOKEY            build key for word 1                         
         XC    SRCKEY,SRCKEY                                                    
         MVI   SRCKTYP,SRCKTYPQ                                                 
         MVC   SRCKCPY,CUXCPY                                                   
         MVC   SRCKUL,LDGAUL                                                    
         MVI   SRCKSUB,SRCKWDSQ                                                 
         MVC   SRCKWRD1,0(RF)                                                   
         CLC   LDGAUL,=C'2P'                                                    
         JNE   SEARCH67                                                         
         CLC   RQ_SRCOF,SPACES     ANY OFFICE PASSED?                           
         JNH   SEARCH67                                                         
         CLI   LDGAOP,1            CHECK OFFICE POSITION 1 ON 2P                
         JNE   SEARCH67                                                         
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   SRCKACT(0),RQ_SRCOF                                              
         EX    RE,0(RF)                                                         
         CLC   RQ_SRCAC,SPACES     ANY ACCOUNT PASSED                           
         JNH   SEARCH67                                                         
         LLC   RE,LDGAL2                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   SRCKACT(0),RQ_SRCAC                                              
         EX    RE,0(RF)                                                         
SEARCH67 MVC   CSVKEY1,SRCKEY                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     SEARCH70                                                         
SEARCH68 GOTOR (#IOEXEC,AIOEXEC),'IOSQ+IODIR+IO1'                               
SEARCH70 JNE   SEARCH66                                                         
         CLC   SRCKEY(SRCKWRD1-SRCKEY),CSVKEY1                                  
         JNE   SEARCH66                                                         
         MVC   CSVKEY1,IOKEY                                                    
         XR    RF,RF                                                            
         IC    RF,XL#LIND                                                       
         SHI   RF,1                                                             
         LR    R1,RF                                                            
         MHI   RF,RQ_SRCS2-RQ_SRCS1                                             
         LA    RF,RQ_SRCS1(RF)                                                  
         LA    RE,L'RQ_SRCW1       WHOLE WORD                                   
         CLI   0(RF),YESQ                                                       
         JNE   *+8                                                              
         IC    RE,XL#SRW1(R1)      OR PREFIX                                    
         CHI   RE,L'SRCKWRD1                                                    
         JNH   *+8                                                              
         LA    RE,L'SRCKWRD1                                                    
         SHI   RE,1                                                             
         SHI   RF,RQ_SRCS1-RQ_SRCW1                                             
         BASR  R1,0                                                             
         CLC   SRCKWRD1(0),0(RF)                                                
         EX    RE,0(R1)                                                         
         JNE   SEARCH68                                                         
         GOTOR FLTSRC           filter on search dir key                        
         JNE   SEARCH68                                                         
                                                                                
         MVC   WORK(2),SRCKUL            Unit/ledger code                       
         MVC   WORK+2(L'SRCKACT),SRCKACT Account code                           
         MVC   WORK+2+L'SRCKACT(L'XL#AOFF),XL#AOFF                              
         GOTOR FLTACT,'XL#SRCH'          filter against limit list              
         JE    SEARCH72                                                         
         CLI   XL#LIML,YESQ        search across whole agency                   
         JE    SEARCH72            yes - ignore approver routine                
         CLI   XL#APPR,YESQ                                                     
         JNE   SEARCH68                                                         
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         J     SEARCH68                                                         
*                                                                               
SEARCH72 CLI   XL#APPR,YESQ                                                     
         JNE   SEARCH74                                                         
         MVC   IOKEY,CSVKEY1                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
SEARCH74 MVI   XL#CWNO,WORD1Q                                                   
         CLI   XL#LIND,2                                                        
         JNE   *+8                                                              
         MVI   XL#CWNO,WORD2Q                                                   
         CLI   XL#LIND,3                                                        
         JNE   *+8                                                              
         MVI   XL#CWNO,WORD3Q                                                   
         CLI   RQ_SRMED,C' '       media code filter                            
         JNH   SEARCH76                                                         
         XR    RE,RE                                                            
         IC    RE,PPROLEN                                                       
         LA    RE,SRCKACT(RE)                                                   
         JNE   SEARCH68                                                         
SEARCH76 GOTOR PUTSRC           put record to table (sorted)                    
         JE    SEARCH68                                                         
         MVC   FULL2,=AL2(AE$TMIIL)                                             
         J     EXITN                                                            
         DROP  R2                                                               
*                                                                               
SEARCH78 OC    XL#CWNO,XL#CWNO                                                  
         JZ    SEARC210                                                         
         MVC   DUB2(4),AGENAREA    All words processed, now put out             
*                                                                               
SEARCH80 GOTOR GETSRC              get entry (R2 points to entry)               
         JNE   SEARC210            (end of entries)                             
*                                                                               
         USING WORDTD,R2                                                        
         MVC   XL#AOFF,SPACES                                                   
         MVC   XL#COFF,WORDOFF     RESTORE OFFICE                               
         CLI   RQ_SRCTY,C'A'       AND search - take all entries                
         JE    SEARCH82                                                         
         DS    0H                  OR search - check all words present          
         MVI   BYTE1,WORD1Q        need word 1                                  
         CLI   XL#SRW2,0                                                        
         JE    *+8                                                              
         OI    BYTE1,WORD2Q        need word 2                                  
         CLI   XL#SRW3,0                                                        
         JE    *+8                                                              
         OI    BYTE1,WORD3Q        need word 3                                  
         CLC   BYTE1,WORDWS        matrch on entry                              
         JNE   SEARCH80                                                         
*                                                                               
SEARCH82 ICM   R1,15,IOADDR        pass D/A and read record                     
         JZ    SEARCH84                                                         
         AHI   R1,IODDWQ                                                        
         MVC   0(L'IODA,R1),WORDDA                                              
SEARCH84 MVC   IODA,WORDDA                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JNE   SEARCH80            Bad master record - ignore                   
         USING ACTRECD,R3                                                       
         L     R3,AIO1                                                          
         GOTOR TSTSET              test on mst record and set name              
         JNE   SEARCH80                                                         
         LR    R1,R3                                                            
         GOTOR ESTCHK                                                           
         JNE   SEARCH80                                                         
         TM    ACTRSTAT,ACTSDELT   Don't return deleted accounts                
         JNZ   SEARCH80                                                         
*                                                                               
         LA    R0,SR_VALS          put out entry (clear values)                 
         LHI   R1,SR_LNQ                                                        
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         MVC   SVIOADDR,IOADDR                                                  
*                                                                               
         MVC   SR_NAM,TEMP2                                                     
         MVC   SR_ULA(2),LDGAUL                                                 
         MVC   SR_ULA+2(12),WORDAC                                              
         MVI   SR_ANAD,NOQ                                                      
         MVI   SR_ANAP,NOQ                                                      
         MVI   SR_ANAM,NOQ                                                      
*                                                                               
         USING RSTELD,R4                                                        
         LA    R4,ACTRFST                                                       
         XR    R0,R0                                                            
         XC    AADREL,AADREL                                                    
         XC    AOATEL,AOATEL                                                    
         MVI   BYTE4,NOQ                                                        
SEARC094 CLI   RSTEL,0                                                          
         JE    SEARC152                                                         
         CLI   RSTEL,RSTELQ        CHECK BILLING STATUSES                       
         JE    SEARC100                                                         
         CLI   RSTEL,JOBELQ                                                     
         JE    SEARC116                                                         
         CLI   RSTEL,SPAELQ                                                     
         JE    SEARC122                                                         
         CLI   RSTEL,ASTELQ                                                     
         JE    SEARC128                                                         
         CLI   RSTEL,DEXELQ                                                     
         JE    SEARC136                                                         
         CLI   RSTEL,ADRELQ                                                     
         JE    SEARC124                                                         
         CLI   RSTEL,OATELQ                                                     
         JE    SEARC126                                                         
         CLI   RSTEL,FFTELQ                                                     
         JE    SEARC138                                                         
                                                                                
SEARC096 IC    R0,RSTLN                                                         
         AR    R4,R0                                                            
         J     SEARC094                                                         
*                                                                               
SEARC100 MVC   SR_CSTG,RSTCOSTG                                                 
         CLI   SR_CSTG,X'80'       unexplained unprintable chars in             
         JNE   *+8                    many cli/pro records                      
         MVI   SR_CSTG,C' '                                                     
         TM    RSTSTAT1,RSTSEADD                                                
         JZ    *+8                                                              
         MVI   SR_ANAD,YESQ                                                     
         TM    RSTSTAT1,RSTSGPEI                                                
         JZ    *+8                                                              
         MVI   SR_ANAP,YESQ                                                     
*&&UK                                                                           
         TM    RSTSTAT2,RSTSMILE                                                
         JZ    *+8                                                              
         MVI   SR_ANAM,YESQ                                                     
*&&                                                                             
         CLC   RQ_SRCUL,PRODUL                                                  
         JE    SEARC104                                                         
         MVI   SR_FPT,NOQ                                                       
         MVI   SR_FJT,NOQ                                                       
         MVI   SR_SGAP,NOQ                                                      
         MVI   SR_AQAP,NOQ                                                      
         TM    RSTSTAT4,RSTSJREA   TEST JOB REQ'D FOR EXP ANALYSIS              
         JZ    *+8                                                              
         MVI   SR_FJT,YESQ                                                      
                                                                                
         CLI   RSTLN,RSTLN3Q       CHECK RIGHT LENGTH ELEMENT                   
         JL    SEARC102                                                         
         TM    RSTSTAT5,RSTSPREA   TEST PRODUCT REQUIRED                        
         JZ    *+8                                                              
         MVI   SR_FPT,YESQ                                                      
*&&US                                                                           
         TM    RSTSTAT7,RSTGAPYN   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   SR_SGAP,YESQ                                                     
         TM    RSTSTAT7,RSTGAPAQ   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   SR_AQAP,YESQ                                                     
*&&                                                                             
*&&UK                                                                           
         TM    RSTSTAT7,RSTGAPQN   NO GAP SUPPLIER CONTROLS                     
         JZ    *+8                                                              
         MVI   SR_SGAP,NOQ                                                      
         TM    RSTSTAT7,RSTGAPAN   NO ACKNOWLEDGE/QUERY                         
         JZ    *+8                                                              
         MVI   SR_AQAP,NOQ                                                      
         TM    RSTSTAT7,RSTGAPQY   GAP SUPPLIER CONTROLS                        
         JZ    *+8                                                              
         MVI   SR_SGAP,YESQ                                                     
         TM    RSTSTAT7,RSTGAPAY   ACKNOWLEDGE/QUERY                            
         JZ    *+8                                                              
         MVI   SR_AQAP,YESQ                                                     
*&&                                                                             
*                                                                               
SEARC102 GOTOR SUPGAP,DMCB,ACTKUNT,SR_SGAP,SR_AQAP                              
*                                                                               
SEARC104 CLI   RSTLN,RSTLN3Q      CHECK ELEMENT RIGHT LENGTH                    
         JL    SEARC106                                                         
         CLI   RQ_SRAPL,RQ_SRORD  ARE WE IN ORDERS?                             
         JNE   SEARC106                                                         
         TM    RSTLSTAT,RSTLSORQ                                                
         JZ    SEARC096           JOB NOT LOCKED                                
         J     SEARCH80           JOB LOCKED                                    
*                                                                               
SEARC106 CLI   RQ_SRAPL,RQ_SRTIM  ARE WE IN TIMESHEETS                          
         JNE   SEARC112                                                         
         MVI   SR_FUTA,NOQ                                                      
         CLI   RSTLN,RSTLN3Q      CHECK ELEMENT RIGHT LENGTH                    
         JL    SEARC108                                                         
         TM    RSTSTAT7,RSTSFUTM  FUTURE TIME ALLOWED?                          
         JZ    SEARC108                                                         
         MVI   SR_FUTA,YESQ                                                     
         J     SEARC110                                                         
SEARC108 CLI   RQ_SRFTM,YESQ      DO WE ONLY WANT FUTURE TIME ACCOUNTS          
         JE    SEARCH80                                                         
         CLI   RSTLN,RSTLN3Q      CHECK ELEMENT RIGHT LENGTH                    
         JL    SEARC096                                                         
SEARC110 MVI   SR_DLOCB,NOQ       SET NOT ALLOWED MATERIALS IN TIME             
         TM    RSTLSTAT,RSTLSBIQ  LOCKED FROM BILLING?                          
         JZ    *+12                                                             
         MVI   SR_DLOCB,YESQ      SET NOT ALLOWED MATERIALS IN TIME             
         J     SEARC096                                                         
         TM    RSTLSTAT,RSTLSTIQ                                                
         JZ    SEARC096           JOB NOT LOCKED                                
         J     SEARCH80           JOB LOCKED                                    
*                                                                               
SEARC112 CLI   RSTLN,RSTLN3Q      CHECK ELEMENT RIGHT LENGTH                    
         JL    SEARC096                                                         
         CLI   RQ_SRAPL,RQ_SREST  ARE WE IN ESTIMATES?                          
         JE    *+12                                                             
         CLI   RQ_SRAPL,RQ_SREJL  ARE WE IN ESTIMATES JOB BILLING?              
         JNE   SEARC114                                                         
         TM    RSTLSTAT,RSTLSESQ                                                
         JZ    SEARC096           JOB NOT LOCKED?                               
         J     SEARCH80           JOB LOCKED                                    
*                                                                               
SEARC114 CLI   RQ_SRAPL,RQ_SREXP  ARE WE IN EXPENSES?                           
         JNE   SEARC096                                                         
         TM    RSTLSTAT,RSTLSEXQ                                                
         JZ    SEARC096           JOB NOT LOCKED?                               
         J     SEARCH80           JOB LOCKED                                    
*                                                                               
         USING JOBELD,R4                                                        
SEARC116 OC    RQ_SRMJO,RQ_SRMJO  ANYTHING PASSED?                              
         JZ    SEARC120                                                         
         CLI   RQ_SRMJO,YESQ      WANT ONLY MASTER JOBS?                        
         JNE   SEARC118                                                         
         TM    JOBSTA2,JOBSMST    THEN SKIP IF NOT MASTER                       
         JZ    SEARCH80                                                         
         J     SEARC120                                                         
*                                                                               
SEARC118 TM    JOBSTA2,JOBSMST    NO MASTER THEN SKIP IF MASTER JOB             
         JNZ   SEARCH80                                                         
         J     SEARC120                                                         
*                                                                               
SEARC120 MVI   SR_MJO,NOQ                                                       
         TM    JOBSTA2,JOBSMST                                                  
         JZ    *+8                                                              
         MVI   SR_MJO,YESQ        SET MASTER JOB                                
         J     SEARC096                                                         
         DROP  R4                                                               
*                                                                               
         USING SPAELD,R4                                                        
SEARC122 CLI   SPATYPE,SPATMJOB                                                 
         JNE   SEARC096                                                         
         MVC   SR_MJC,SPAAACT                                                   
         CLI   RQ_SRMJO,NOQ        DO WE NOT WANT MASTER AND SUB JOBS           
         JE    SEARCH80            YES                                          
         J     SEARC096                                                         
*                                                                               
         USING ADRELD,R4                                                        
SEARC124 ST    R4,AADREL                                                        
         J     SEARC096                                                         
*                                                                               
         USING OATELD,R4                                                        
SEARC126 CLI   RQ_SRAPL,RQ_SRORD                                                
         JNE   SEARC096                                                         
         CLI   OATSUB,OATSUB5Q                                                  
         JNE   SEARC096                                                         
         ST    R4,AOATEL                                                        
         J     SEARC096                                                         
*                                                                               
         USING ASTELD,R4                                                        
SEARC128 CLC   AGYCURR,ASTCUR                                                   
         JE    SEARC130                                                         
         MVC   SR_CUR,ASTCUR                                                    
         OC    SR_CUR,SPACES                                                    
         CLI   ASTCUR,ASTCANY                                                   
         JNE   SEARC130                                                         
         MVC   SR_CUR,=CL3'***'                                                 
SEARC130 MVI   SR_13BVI,NOQ                                                     
         MVI   SR_ISFO,NOQ                                                      
         TM    ASTSTAT1,ASTISFOR   Is foreign language user                     
         JZ    *+8                                                              
         MVI   SR_ISFO,YESQ                                                     
         CLI   RQ_SRAPL,RQ_SRINV   TEST INVOICES                                
         JNE   SEARC096                                                         
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         JNE   SEARC096                                                         
*&&UK                                                                           
         CLI   AST13VAT,C' '       TEST VAT CODE SET                            
         JNH   *+14                                                             
         MVC   SR_VATC(1),AST13VAT                                              
*&&                                                                             
         MVI   SR_13BVI,YESQ                                                    
         CLI   ASTKSVTY,0          TEST KSV TYPE SET                            
         JNH   SEARC096                                                         
         MVC   FULL1,XL#TODP       SET TODAY'S DATE                             
         CLC   RQ_SRIDT,SPACES     TEST INVOICE DATE PASSED                     
         JNH   SEARC132                                                         
         GOTOR VDATCON,DMCB,(0,RQ_SRIDT+2),(1,FULL1)                            
SEARC132 LA    RF,WORK             GET RATE FOR KSV TYPE                        
         USING CONBLKD,RF                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONFLD,CONFKSV      KSV CALL                                     
         MVI   CONACTN,CONAGETQ    GET KSV DETAILS                              
         MVI   CONILEN,L'ASTKSVTY  LENGTH OF CODE                               
         LA    RE,ASTKSVTY                                                      
         STCM  RE,15,CONIADD       INPUT ADDRESS                                
         LA    RE,TEMP                                                          
         STCM  RE,15,CONOADD       OUTPUT ADDRESS                               
         MVC   CONCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   CONKSVDT,FULL1      EFFECTIVE KSV DATE                           
         GOTO1 VCONVERT,CONBLK                                                  
         JE    *+14                                                             
SEARC134 MVC   LP_ERROR,=AL2(AE$NOKSV)                                          
         J     XERROR                                                           
         DROP  RF                                                               
         ZAP   DUB1,TEMP(3)                                                     
         CURED (P8,DUB1),(L'SR_KSVR,SR_KSVR),0,ZERO=YES,ALIGN=LEFT              
         J     SEARC096                                                         
*                                                                               
         USING DEXELD,R4                                                        
SEARC136 CLI   RQ_SRAPL,RQ_SRINV   TEST INVOICES                                
         JNE   SEARC096                                                         
         GOTOR GETDDF,DMCB,DEXELD,SR_DDXP  RETURN DUE DATE FORMULA              
         J     SEARC096                                                         
                                                                                
         USING FFTELD,R4                                                        
SEARC138 DS    0H                                                               
*&&UK                                                                           
         CLI   FFTTYPE,FFTTVATC    VAT CODE                                     
         JE    *+12                                                             
         CLI   FFTTYPE,FFTTG13B    13B VAT CODE                                 
         JNE   SEARC140                                                         
         CLI   RQ_SRAPL,RQ_SRINV   TEST INVOICES                                
         JNE   SEARC096                                                         
         CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         JNE   SEARC096                                                         
         LA    RF,SR_VATC                                                       
         CLI   SR_VATC,C' '        MAY BE A 13B CODE FROM ASTEL ALREADY         
         JNH   *+8                                                              
         AHI   RF,1                SO APPEND OTHERS                             
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   0(0,RF),FFTDATA     UP TO FOUR MORE                              
         EX    RE,0(R1)                                                         
         J     SEARC096                                                         
*&&                                                                             
SEARC140 DS    0H                                                               
*&&US                                                                           
SEARC142 CLI   FFTTYPE,FFTTBEML    SEARCH FOR ORDER AUTHORISER IF NOT           
         JNE   SEARC144            FOUND USE EMAIL ON 1ST PAGE                  
         MVI   BYTE4,YESQ                                                       
         J     SEARC146                                                         
*&&                                                                             
SEARC144 CLI   BYTE4,YESQ          DID WE FIND ORDER AUTHORISER                 
         JE    SEARC148            YES                                          
*&&UK*&& CLI   FFTTYPE,FFTTPEML    EMAIL?                                       
*&&US*&& CLI   FFTTYPE,FFTTEML                                                  
         JNE   SEARC148                                                         
SEARC146 MVC   SR_EMADD,SPACES                                                  
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   SR_EMADD(0),FFTDATA                                              
         EX    RF,0(R1)                                                         
         J     SEARC096                                                         
                                                                                
SEARC148 CLI   FFTTYPE,FFTTPFAX    FAX?                                         
         JNE   SEARC150                                                         
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   SR_FAX(0),FFTDATA                                                
         EX    RF,0(R1)                                                         
         J     SEARC096                                                         
                                                                                
SEARC150 CLI   FFTTYPE,FFTTPTEL    TELEPHONE?                                   
         JNE   SEARC096                                                         
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   SR_TELE(0),FFTDATA                                               
         EX    RF,0(R1)                                                         
         J     SEARC096                                                         
                                                                                
SEARC152 L     R4,AADREL                                                        
         OC    AOATEL,AOATEL                                                    
         JZ    SEARC156                                                         
         L     R4,AOATEL                                                        
         USING OATELD,R4                                                        
*&&UK                                                                           
         XR    R1,R1                                                            
         ICM   R1,1,OATNUM         N'ADDRESS LINES                              
         JZ    SEARC160                                                         
         CHI   R1,4                                                             
         JNH   *+8                                                              
         LA    R1,4                                                             
         LA    RF,OATADD1                                                       
         LA    RE,SR_ADDR1                                                      
         J     SEARC158                                                         
*&&                                                                             
*&&US                                                                           
         LA    R1,2                                                             
         LA    RF,OATLINE1                                                      
         LA    RE,SR_ADDR1                                                      
         TM    OATSTAT,OATCSZ                                                   
         JZ    SEARC154                                                         
         MVC   SR_ADDR3(L'OATLINE1),OATCSZZR                                    
                                                                                
SEARC154 MVC   0(L'OATLINE1,RE),0(RF)                                           
         AHI   RE,L'OATLINE1                                                    
         AHI   RF,L'OATLINE1                                                    
         JCT   R1,SEARC154                                                      
         J     SEARC160                                                         
*&&                                                                             
         USING ADRELD,R4                                                        
SEARC156 XR    R1,R1                                                            
         ICM   R1,1,ADRNUM         N'ADDRESS LINES                              
         JZ    SEARC160                                                         
         CHI   R1,4                                                             
         JNH   *+8                                                              
         LA    R1,4                                                             
         LA    RF,ADRADD1                                                       
         LA    RE,SR_ADDR1                                                      
                                                                                
SEARC158 MVC   0(L'ADRADD1,RE),0(RF)                                            
         AHI   RE,L'OATLINE1                                                    
         AHI   RF,L'ADRADD1                                                     
         JCT   R1,SEARC158                                                      
                                                                                
SEARC160 MVI   SR_DRA,NOQ                                                       
         MVI   SR_CLO,NOQ                                                       
         MVI   SR_LOC,NOQ                                                       
         TM    ACTRSTAT,ACTSDRFT                                                
         JZ    *+8                                                              
         MVI   SR_DRA,YESQ                                                      
         TM    ACTRSTAT,ACTSCLOS                                                
         JZ    *+8                                                              
         MVI   SR_CLO,YESQ                                                      
         TM    ACTRSTAT,ACTSLOCK                                                
         JZ    *+8                                                              
         MVI   SR_LOC,YESQ                                                      
         CLI   HALF1,FF            pass VAT rate on SG                          
         JE    SEARC162                                                         
         CURED (B2,HALF1),(L'SR_VAT,SR_VAT),0,ZERO=YES,ALIGN=LEFT               
*                                                                               
SEARC162 CLI   HALF2,FF            pass discount rate if supplier               
         JE    SEARC164                                                         
         CURED (B2,HALF2),(L'SR_DISC,SR_DISC),0,ZERO=YES,ALIGN=LEFT             
*                                                                               
SEARC164 CLC   =C'ST',ACTKULA                                                   
         JNE   SEARC166                                                         
         MVC   SAVEKEY,IOKEY      SAVE OFF KEY                                  
         GOTOR GETAGN,DMCB,SR_ULA,SR_AGA,SR_AGAD1                               
         MVC   IOKEY,SAVEKEY       SAVE OFF KEY                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                 REREAD RECORD TO RESTORE READ SEQ            
         DC    H'0'                                                             
*                                                                               
SEARC166 CLC   =C'1R',ACTKULA                                                   
         JNE   SEARC172                                                         
         CLI   XL#APPL,RQ_SREST    Estimates?                                   
         JE    *+20                                                             
         CLI   XL#APPL,RQ_SREXP    Expenses?                                    
         JE    *+12                                                             
         CLI   XL#APPL,RQ_SRTIM    Time?                                        
         JNE   SEARC170                                                         
         CLI   XL#CLVL,X'04'                                                    
         JNE   SEARC170                                                         
         CLI   XL#APPL,RQ_SREXP    Expenses?                                    
         JNE   SEARC168            Only interested in profile for exps          
         GOTOR (#CSTPRF,ACSTPRF),DMCB,ACTKACT                                   
         L     R4,ACOBLOCK                                                      
         USING COBLOCKD,R4                                                      
         MVC   SR_XAPP,COCAE                                                    
         MVC   SR_XMIL,COMIL                                                    
         DROP  R4                                                               
SEARC168 XR    RE,RE                                                            
         IC    RE,LDGAL3                                                        
         MVC   SR_ULA,SPACES                                                    
         LA    RF,ACTKACT                                                       
         AR    RE,RF                                                            
         MVC   SR_ACT,0(RE)                                                     
         MVC   OB_KEY(L'SR_ACT),SR_ACT                                          
         GOTOR GETBUF,OB_D         Do we already have this entry                
         JE    SEARCH80            Yes                                          
         GOTOR ADDBUF,OB_D         Add person code to buffer so we              
*                                      don't get duplicates                     
SEARC170 DS    0H                  return 1R off/dep/sub details                
         GOTOR RETURN1R,ACTRECD                                                 
         J     SEARC206                                                         
                                                                                
SEARC172 CLC   ACTKULA(2),PRODUL   IS IT SJ?                                    
         JNE   SEARC206                                                         
         MVC   SAVEKEY,IOKEY      SAVE OFF KEY                                  
         LA    R4,ACTKULA+L'PRODUL                                              
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   SR_CLI(0),0(R4)                                                  
         EX    RE,0(RF)                                                         
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'PRODUL),PRODUL                                           
         BASR  RF,0                                                             
         MVC   TEMP2+L'PRODUL(0),0(R4)                                          
         EX    RE,0(RF)                                                         
         AHI   RE,1                                                             
         AR    R4,RE                                                            
         GOTOR (#GETACN,AGETACN)                                                
         MVC   SR_CLINM,TEMP2      SAVE OFF CLIENT NAME                         
         LLC   RE,LDGAL2                                                        
         LLC   RF,LDGAL1                                                        
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         CLC   0(0,R4),SPACES                                                   
         EX    RE,0(R1)                                                         
         JE    SEARC174            CLIENT ONLY SEARCH                           
         BASR  R1,0                                                             
         MVC   SR_PRO(0),0(R4)     CHECK IF CLIENT N PRODUCT SEARCH             
         EX    RE,0(R1)                                                         
                                                                                
         LLC   RE,LDGAL2                                                        
         SHI   RE,1                                                             
         LA    RF,ACTKULA+L'PRODUL                                              
         MVC   TEMP2,SPACES                                                     
         MVC   TEMP2(L'PRODUL),PRODUL                                           
         BASR  R1,0                                                             
         MVC   TEMP2+L'PRODUL(0),0(RF)                                          
         EX    RE,0(R1)                                                         
         AHI   RE,1                                                             
         AR    R4,RE                                                            
         GOTOR (#GETACN,AGETACN)                                                
         MVC   SR_PRONM,TEMP2      SAVE OFF PRODUCT NAME                        
*                                                                               
SEARC174 MVC   IOKEY,SAVEKEY       SAVE OFF KEY                                 
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                 REREAD RECORD TO RESTORE READ SEQ            
         DC    H'0'                                                             
         MVC   SR_OFFC,XL#COFF                                                  
         CLC   XL#COFF,SPACES                                                   
         JH    SEARC176                                                         
         MVC   SR_OFFC,XL#AOFF                                                  
         CLC   XL#AOFF,SPACES                                                   
         JH    SEARC176                                                         
         MVC   SR_OFFC,XL#HOFF                                                  
         CLC   XL#HOFF,SPACES                                                   
         JH    SEARC176                                                         
         J     SEARC178                                                         
*                                                                               
SEARC176 MVC   TEMP2,SPACES        GET OFFICE NAME                              
         MVC   TEMP2(2),=C'2D'                                                  
         MVC   TEMP2+2(L'SR_OFFC),SR_OFFC                                       
         MVC   SAVEKEY,IOKEY                                                    
         GOTOR (#GETACN,AGETACN)                                                
         MVC   SR_OFFN,TEMP2                                                    
         MVC   IOKEY,SAVEKEY                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SEARC178 MVC   IOADDR,SVIOADDR                                                  
         MVC   SJACLIC,SPACES                                                   
         MVC   SJAPROC,SPACES                                                   
         MVC   SJAJOBC,SPACES                                                   
         LLC   RE,PCLILEN                                                       
         SHI   RE,1                                                             
         BASR  R1,0                                                             
         MVC   SJACLIC(0),ACTKACT                                               
         EX    RE,0(R1)                                                         
         LA    RE,ACTKACT+1(RE)                                                 
         LLC   R1,PCLILEN                                                       
         LLC   RF,PPROLEN                                                       
         SR    RF,R1                                                            
         SHI   RF,1                                                             
         BASR  R1,0                                                             
         MVC   SJAPROC(0),0(RE)                                                 
         EX    RF,0(R1)                                                         
         AR    RE,RF                                                            
         AHI   RE,1                                                             
         LLC   RF,PPROLEN                                                       
         LLC   R1,PJOBLEN                                                       
         SR    R1,RF                                                            
         SHI   R1,1                                                             
         BASR  RF,0                                                             
         MVC   SJAJOBC(0),0(RE)                                                 
         EX    RE,0(RF)                                                         
*                                                                               
         USING GOBLOCK,R2                                                       
         L     R2,AGOBLOCB                                                      
                                                                                
         XC    GOADM(GOABUFF-GOADM),GOADM                                       
         XC    GOADETS(GOACOVL-GOADETS),GOADETS                                 
                                                                                
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOAEXT,AGOXBLCK     US USES 1ST EXTENSION BLOCK                  
         MVC   GOABEXT,AGOBBLCK    UK USES 2ND EXTENSION BLOCK                  
         MVC   GOABUFF,ATWA                                                     
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JNZ   *+10                                                             
         MVC   GOABUFF,ATIA                                                     
         LHI   RE,GENAEXTN-GENAREA                                              
         ST    RE,GOLBUFF                                                       
                                                                                
         MVC   GOSELCUL(1),CUXCPY                                               
         MVC   GOSELCUL+1(2),PRODUL                                             
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELCLI(L'SJACLIC),SJACLIC                                      
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO(L'SJAPROC),SJAPROC                                      
         MVC   GOSELJOB,SJAJOBC                                                 
         MVC   GOCTRY,CUCTRY                                                    
         CLI   RQ_SRMED,C' '                                                    
         JNH   *+10                                                             
         MVC   GOSELMED,RQ_SRMED                                                
         CLI   RQ_SRWC,C' '                                                     
         JNH   *+14                                                             
         MVC   GOSELWC,RQ_SRWC                                                  
         MVI   GOANYWC,YESQ                                                     
         L     RF,ACOMFACS                                                      
                                                                                
         XC    GOACOVL,GOACOVL                                                  
         TM    LP_FLAG,LP_FOFFL    TEST OFFLINE                                 
         JZ    *+10                                                             
         MVC   GOACOVL,VCOVAIL                                                  
         MVC   GOABINSR,CBINSRCH-COMFACSD(RF)                                   
                                                                                
         GOTO1 VGETOPT,DMCB,GOBLOCKD                                            
                                                                                
*&&US*&& MVI   SR_ISFO,C'N'                                                     
*&&UK                                                                           
         CLI   XL#LIND,3                                                        
         JNL   *+10                                                             
         MVC   SR_ISFO,GOJULDEF                                                 
         MVC   SR_BKSV,GOBILKSV                                                 
*&&                                                                             
         MVC   SR_NAE,GONEEDAE                                                  
         MVC   SR_BILTY,GOBILTYP                                                
         USING GOXBLKD,R2                                                       
         L     R2,AGOXBLCK                                                      
         MVC   SR_GAP,GOGAPS                                                    
         LLC   RF,GOGDES                                                        
         CVD   RF,DUB                                                           
         MVC   SR_DFTEX,DUB+6                                                   
         LLC   RF,GODNDV                                                        
         CVD   RF,DUB                                                           
         MVC   SR_DFTND,DUB+6                                                   
         MVC   SR_GAPAR,GOGARA                                                  
         MVC   SR_SWPD,GOSWPD                                                   
         MVC   SR_GAPEX,GOGEMX                                                  
         MVC   SR_GAPED,GOGEMD                                                  
*                                                                               
         CLI   RQ_SRAPL,RQ_SRTIM      ONLY FOR TIMESHEETS                       
         JNE   SEARC194                                                         
         CLI   RQ_SRFPT,YESQ          PER2 OVERRIDE FOR SHOWING PRODUCT         
         JNE   SEARC180                                                         
         MVI   SR_FPT,C'A'            FORCE PRODUCT                             
         J     SEARC182                                                         
*                                                                               
SEARC180 MVC   SR_FPT,GOFPT                                                     
*                                                                               
SEARC182 CLI   RQ_SRFJT,YESQ          PER2 OVERRIDE FOR SHOWING JOB             
         JNE   SEARC184                                                         
         MVI   SR_FJT,C'A'            FORCE JOB                                 
         J     SEARC186                                                         
*                                                                               
SEARC184 MVC   SR_FJT,GOFJT                                                     
*                                                                               
SEARC186 MVC   SR_DEFT,GOTOT                                                    
                                                                                
*&&UK                                                                           
         USING GOBLOCK,R2                                                       
         L     R2,AGOBLOCB                                                      
*&&                                                                             
*&&US                                                                           
         USING GOXBLKD,R2                                                       
         L     R2,AGOXBLCK                                                      
*&&                                                                             
         LA    RE,GOTTALLW                                                      
         LA    R1,3                                                             
         MVC   SR_BILA(L'SR_BILA+L'SR_CHGA+L'SR_NBLA),=C'NNN'                   
SEARC188 CLI   0(RE),C'B'                                                       
         JNE   *+8                                                              
         MVI   SR_BILA,C'Y'                                                     
         CLI   0(RE),C'R'                                                       
         JNE   *+8                                                              
         MVI   SR_CHGA,C'Y'                                                     
         CLI   0(RE),C'N'                                                       
         JNE   *+8                                                              
         MVI   SR_NBLA,C'Y'                                                     
         LA    RE,1(RE)                                                         
         JCT   R1,SEARC188                                                      
                                                                                
         CLI   RQ_SRAPL,RQ_SRTIM      CHECK TIME APPLICATION                    
         JNE   SEARC190                                                         
         CLI   RQ_SRFJT,YESQ                                                    
         JE    *+10                                                             
         MVC   SR_JOBT,GOTNOJOB                                                 
                                                                                
SEARC190 LA    RF,GOTFNARR                                                      
         LA    R1,3                                                             
         MVC   SR_FNTB(L'SR_FNTB+L'SR_FNTR+L'SR_FNTN),=C'NNN'                   
SEARC192 CLI   0(RF),C'B'                                                       
         JNE   *+8                                                              
         MVI   SR_FNTB,C'Y'                                                     
         CLI   0(RF),C'R'                                                       
         JNE   *+8                                                              
         MVI   SR_FNTR,C'Y'                                                     
         CLI   0(RF),C'N'                                                       
         JNE   *+8                                                              
         MVI   SR_FNTN,C'Y'                                                     
         LA    RF,1(RF)                                                         
         JCT   R1,SEARC192                                                      
*                                                                               
         USING GOXBLKD,R2                                                       
SEARC194 L     R2,AGOXBLCK                                                      
         MVC   SR_NJLE,GONJLE                                                   
         MVC   SR_BILO,GOBILO                                                   
         MVC   SR_CCWC,GOCCW      COLOUR CODED WORK CODES                       
         MVC   SR_FPOR,GOFPORES   Fully approved order resubmit rules           
         MVC   SR_PPOR,GOPPORES   Part approved order resubmit rules            
         MVC   SR_MPOR,GOMPORES   Matched order resubmit rules                  
         MVC   SR_WCF,GOWCF       USE WC FLAG FOR ESTIMATE CHECK                
         MVI   SR_MDOWC,C'M'                                                    
*&&UK                                                                           
         CLI   GOICRA,GOIWCSI                                                   
         JL    *+8                                                              
*&&                                                                             
         MVI   SR_MDOWC,C'W'                                                    
         GOTOR GETMED,DMCB,ACTKULA,SR_MINC                                      
*&&UK                                                                           
         CLI   GOICRA,GOIMESK                                                   
         JNE   SEARC196                                                         
*&&                                                                             
         MVC   SR_MINC(L'ACTKUNT+L'ACTKLDG),=C'SK'                              
                                                                                
SEARC196 CLI   RQ_SRAPL,RQ_SRTIM   CHECK TIME APPLICATION                       
         JNE   SEARC198                                                         
         CLI   XL#CLVL,3           CHECK JOB LEVEL REQUEST                      
         JNE   SEARC198                                                         
         CLI   SR_JOBT,NOQ         JOB INPUT ALLOWED ON TIME?                   
         JE    SEARCH80            NO                                           
                                                                                
SEARC198 CLI   RQ_SRAPL,RQ_SRORD   ORDERS                                       
         JE    SEARC200                                                         
         CLI   RQ_SRAPL,RQ_SREXP   EXPENSES                                     
         JE    SEARC200                                                         
         CLI   RQ_SRAPL,RQ_SRINV   INVOICES                                     
         JNE   SEARC202                                                         
SEARC200 CLI   RQ_SRBIL,NOQ        IS THE ENTRY BILLABLE WITHIN MODULE          
         JNE   SEARC202            YES                                          
         CLI   SR_BILO,YESQ        NO - IS IT BILLABLE ONLY                     
         JE    SEARCH80            YES - SKIP THIS ACCOUNT                      
*                                                                               
SEARC202 CLI   RQ_SRAPL,RQ_SREST  Estimates                                     
         JE    *+12                                                             
         CLI   RQ_SRAPL,RQ_SREJL  Estimates                                     
         JNE   SEARC204                                                         
         CLI   SR_DRA,YESQ        Is the job draft                              
         JNE   SEARC204           No                                            
         CLI   GOAEDT,NOQ         Yes - are we allowed draft jobs               
         JE    SEARCH80           No                                            
*                                                                               
         USING GOBBLKD,RF                                                       
SEARC204 L     RF,AGOBBLCK                                                      
         CLC   SR_CUR,SPACES                                                    
         JH    SEARC206                                                         
         CLC   GOBILCUR,AGYCURR                                                 
         JE    SEARC206                                                         
         MVC   SR_CUR,GOBILCUR                                                  
         DROP  RF                                                               
*                                                                               
SEARC206 MVC   IOADDR,SVIOADDR                                                  
         CLC   LDGAUL(2),SGQ                                                    
         JNE   SEARC208                                                         
         CLC   SR_ULA(2),SGQ                                                    
         JNE   SEARC208                                                         
         MVC   TEMP2(14),SR_ULA                                                 
         MVC   SR_ULA,SPACES                                                    
         MVC   SR_ACT,TEMP2+2                                                   
*                                                                               
SEARC208 GOTOR LP_APUTO,LP_D                                                    
         J     SEARCH80                                                         
         DROP  R2,R3                                                            
*                                                                               
SEARC210 DS    0H                  HOUSEKEEPING                                 
         J     EXITY                                                            
*                                                                               
SRWORKD  DSECT                     ** XSEARCH local w/s **                      
SRRBAREA DS    XL(OB_LNQ)                                                       
         ORG   SRRBAREA+(OB_OTHER-OB_D)                                         
         ORG                                                                    
SRWORKL  EQU   *-SRWORKD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET PERSON DETAILS (USE TS#PBLK)                                    *         
***********************************************************************         
                                                                                
PERDTL   NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*PERDTL*'                                                    
                                                                                
         USING PERRECD,R2                                                       
         LA    R2,IOKEY            READ FOR PERSON RECORD                       
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUXCPY                                                   
         MVC   PERKCODE,TEMP2                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    PERDTL10                                                         
         MVC   FULL2(2),=AL2(AE$IVPER)                                          
         J     EXITN                                                            
                                                                                
PERDTL10 GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOMST+IO1'                              
         JE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         L     R2,AIO1                                                          
         LA    R3,PERRFST          LOCATE ELEMENTS                              
         USING EMPELD,R3                                                        
         XR    R0,R0                                                            
                                                                                
PERDTL20 CLI   EMPEL,0                                                          
         JE    PERDTL80                                                         
         CLI   EMPEL,LOCELQ                                                     
         JE    PERDTL60                                                         
         CLI   EMPEL,PIDELQ                                                     
         JE    PERDTL70                                                         
         CLI   EMPEL,EMPELQ                                                     
         JE    PERDTL72                                                         
                                                                                
PERDTL30 IC    R0,EMPLN                                                         
         AR    R3,R0                                                            
         J     PERDTL20                                                         
                                                                                
         USING LOCELD,R3                                                        
         USING OFFALD,R1                                                        
PERDTL60 CLI   CUACCS,0            LIMIT ACCESS?                                
         JE    PERDTL62            NO                                           
         L     R1,AOFFAREA                                                      
         MVC   OFFAOFFC,LOCOFF     VALIDATE CURRENT OFFICE                      
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 VOFFAL                                                           
         JNE   PERDTL30                                                         
         DROP  R1                                                               
                                                                                
PERDTL62 ST    R3,ACLOCL          STORE MOST RECENT LOCATION                    
         CLC   XL#TODP,LOCSTART   FIND LOCATION FOR CURRENT DATE                
         JL    PERDTL30                                                         
         OC    LOCEND,LOCEND                                                    
         JZ    PERDTL65                                                         
         CLC   XL#TODP,LOCEND                                                   
         JH    PERDTL30                                                         
PERDTL65 ST    R3,ALOCEL                                                        
         J     PERDTL30                                                         
                                                                                
         USING PIDELD,R3                                                        
PERDTL70 MVC   X#PPID,PIDNO       EXTRACT PID                                   
         J     PERDTL30                                                         
                                                                                
         USING EMPELD,R3                                                        
PERDTL72 CLI   XL#APPL,RQ_ADEST    FOR ESTIMATES CHECK ACTIVE PERSON            
         JNE   PERDTL30                                                         
         CLI   EMPCSTAT,0          ACTIVE PERSON                                
         JE    PERDTL30            YES - ALWAYS ALLOW                           
         CLI   EMPCSTAT,EMPCTRM                                                 
         JNE   PERDTL74                                                         
         MVC   FULL2(2),=AL2(AE$PRSNT)                                          
         J     EXITN                                                            
PERDTL74 CLI   EMPCSTAT,EMPCLOA                                                 
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   FULL2(2),=AL2(AE$PRSLA)                                          
         J     EXITN                                                            
         DROP  R3                                                               
                                                                                
PERDTL80 OC    ALOCEL,ALOCEL      ENSURE THIS IS SET                            
         JNZ   PERDTL85                                                         
         OC    ACLOCL,ACLOCL      IF CAN'T FIND LOCATION FOR CURRENT            
         JZ    PERDTL82           DATE USE MOST RECENT LOCATION                 
         MVC   ALOCEL,ACLOCL                                                    
         J     PERDTL85                                                         
                                                                                
PERDTL82 MVC   FULL2(2),=AL2(AE$IVLDT)                                          
         J     EXITN                                                            
                                                                                
PERDTL85 OC    X#PPID,X#PPID                                                    
         JNZ   PERDTL90                                                         
         MVC   FULL2(2),=AL2(AE$NCPID)                                          
         J     EXITN                                                            
                                                                                
PERDTL90 J     EXITY                                                            
                                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* Return 1R details in search                                         *         
***********************************************************************         
                                                                                
RETURN1R NTR1  LABEL=*                                                          
         J     *+12                                                             
         DC    CL8'*RET_1R*'                                                    
                                                                                
         USING ACTRECD,R2                                                       
         LR    R2,R1                                                            
         MVC   SAVEKEY,IOKEY       save current key                             
                                                                                
         CLI   XL#CLVL,1           on level 1 request no data so exit           
         JE    RET1RX                                                           
                                                                                
         MVC   SR_1ROFC,SPACES     pass office level                            
         LLC   RE,LDGAL1                                                        
         SHI   RE,1                                                             
         BASR  RF,0                                                             
         MVC   SR_1ROFC(0),ACTKACT                                              
         EX    RE,0(RF)                                                         
                                                                                
         MVC   TEMP2,SPACES        account of this level                        
         LLC   RE,LDGAL1                                                        
         AHI   RE,L'ACTKUNT+L'ACTKLDG-1                                         
         BASR  RF,0                                                             
         MVC   TEMP2(0),ACTKULA                                                 
         EX    RE,0(RF)                                                         
         GOTOR (#GETACN,AGETACN)                                                
         MVC   SR_1ROFN,TEMP2      name of this level                           
                                                                                
         CLI   XL#CLVL,2           if level 2 exit here                         
         JE    RET1R2                                                           
                                                                                
         MVC   SR_1RDEC,SPACES     pass department level                        
         LLC   RF,LDGAL1                                                        
         LLC   RE,LDGAL2                                                        
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         LA    RF,ACTKACT(RF)                                                   
         BASR  R1,0                                                             
         MVC   SR_1RDEC(0),0(RF)                                                
         EX    RE,0(R1)                                                         
                                                                                
         MVC   TEMP2,SPACES        account of this level                        
         LLC   RE,LDGAL2                                                        
         AHI   RE,L'ACTKUNT+L'ACTKLDG-1                                         
         BASR  R1,0                                                             
         MVC   TEMP2(0),ACTKULA                                                 
         EX    RE,0(R1)                                                         
         GOTOR (#GETACN,AGETACN)                                                
         MVC   SR_1RDEN,TEMP2      name of this level                           
                                                                                
         CLI   XL#CLVL,3           if level 3 exit here                         
         JE    RET1R2                                                           
                                                                                
         MVC   SR_1RSDC,SPACES     pass sub department level                    
         LLC   RF,LDGAL2                                                        
         LLC   RE,LDGAL3                                                        
         SR    RE,RF                                                            
         SHI   RE,1                                                             
         LA    RF,ACTKACT(RF)                                                   
         BASR  R1,0                                                             
         MVC   SR_1RSDC(0),0(RF)                                                
         EX    RE,0(R1)                                                         
                                                                                
         MVC   TEMP2,SPACES        account of this level                        
         LLC   RE,LDGAL3                                                        
         AHI   RE,L'ACTKUNT+L'ACTKLDG-1                                         
         BASR  R1,0                                                             
         MVC   TEMP2(0),ACTKULA                                                 
         EX    RE,0(R1)                                                         
         GOTOR (#GETACN,AGETACN)                                                
         MVC   SR_1RSDN,TEMP2      name of this level                           
                                                                                
RET1R2   MVC   IOKEY,SAVEKEY       reread record to restore IO sequence         
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         JE    RET1RX                                                           
         DC    H'0'                                                             
                                                                                
RET1RX   DS    0H                                                               
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* GENERAL EXIT AND DECLARATIONS                                      *          
**********************************************************************          
                                                                                
XERROR   DS    0H                                                               
         MVI   LP_RMODE,LP_RERRR                                                
         MVI   LP_EMSYS,6                                                       
         J     EXITN                                                            
                                                                                
EXITN    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITNX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITNX   LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    TM    TWAMODE,TWAMUWD                                                  
         JZ    EXITYX                                                           
         NI    TWAMODE,FF-TWAMUWD                                               
         DC    H'0'                Unwind here|                                 
EXITYX   LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
TSTABO2  EQU   C'Y'                                                             
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
WORKLEN  DC    AL2(((WORKL+7)/8)*8)                                             
*                                                                               
APPLTAB  DS    0XL1                                                             
         DC    AL1(RQ_ADTIM,LIDLTIME)                                           
APPLTABL EQU   *-APPLTAB                                                        
         DC    AL1(RQ_ADORD,LIDLORDS)                                           
         DC    AL1(RQ_ADEST,LIDLESTM)                                           
         DC    AL1(RQ_ADEJL,LIDLESTM)                                           
         DC    AL1(RQ_ADEXP,LIDLEXPN)                                           
         DC    AL1(RQ_ADJOB,LIDLJOBS)                                           
         DC    AL1(RQ_ADREP,LIDLREPT)                                           
         DC    AL1(RQ_ADINV,LIDLINVC)                                           
         DC    AL1(RQ_ADRES,LIDLRESC)                                           
         DC    AL1(RQ_ADDAS,LIDLJOBS)                                           
         DC    X'FF'                                                            
                                                                                
SRCHTAB  DC    AL1(RQ_SRCW1-RQ_DATA,RQ_SRCS1-RQ_DATA)                           
         DC    AL1(RQ_SRCW2-RQ_DATA,RQ_SRCS2-RQ_DATA)                           
         DC    AL1(RQ_SRCW3-RQ_DATA,RQ_SRCS3-RQ_DATA)                           
         DC    AL1(EOF)                                                         
                                                                                
DPATYPS  DS    0X                                                               
         DC    AL1(RQ_ADTIM,DPAPATIM,0,0,0,0,0)                                 
         DC    X'FF'                                                            
DPATYPSL EQU   *-DPATYPS                                                        
         DC    AL1(RQ_ADEXP,DPAPAEXP,DPAPAEXF,DPAPAEX1)                         
         DC    AL1(DPAPAEX2,DPAPAEB1,DPAPAEB2)                                  
         DC    X'FF'                                                            
         DC    X'FF'                                                            
*                                                                               
DMKEY    DC    CL8'DMKEY'                                                       
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
ACCARC   DC    CL8'ACCARC'                                                      
SGQ      DC    CL2'SG'                                                          
SVQ      DC    CL2'SV'                                                          
SFQ      DC    CL2'SF'                                                          
STQ      DC    CL2'ST'                                                          
PZERO    DC    P'0'                                                             
         EJECT                                                                  
                                                                                
SAVED    DSECT                                                                  
OVERLAY  DS    D                                                                
AMASTC   DS    A                   A(MASTC)                                     
VACCEMU  DS    A                                                                
APRINTER DS    A                   A(PRINTER) - offline                         
ARCPRINT DS    A                   A(RCPRINT)                                   
ABUFFRIN DS    AL4                 A(BUFFERIN)                                  
                                                                                
RUNMODE  DS    XL(L'RUNPMODE)      DDLINK/RUNNER calling mode                   
                                                                                
TSARABUF DS    XL(TSPXTNL)         TSAR block for account list                  
                                                                                
DVALUES  DS    0D                                                               
SVIOADDR DS    A                                                                
SAVER1   DS    F                   Save register 1                              
ACLOCL   DS    F                   saved address of most recent loctn           
ALOCEL   DS    F                   saved location element                       
AADREL   DS    F                   saved address element                        
AOATEL   DS    F                   saved other address element                  
SAVEKEY  DS    XL(L'IOKEY)         Overlay save key area                        
SAVEDPOS DS    XL1                 SAVED DEP POSITION                           
SAVEDLEN DS    XL1                 SAVED DEP LENGTH                             
SAVEPOS2 DS    XL1                 SAVED OFF POSITION                           
SAVELEN2 DS    XL1                 SAVED OFF LENGTH                             
*                                                                               
XL#TODP  DS    XL3                 Today's date packed                          
XL#GLIND DS    XL1                                                              
XL#GLIPQ EQU   X'80'                                                            
XL#GLIFQ EQU   X'08'                                                            
XL#LIND  DS    XL1                                                              
XL#HIGH  DS    XL1                                                              
XL#IOMD  DS    XL1                                                              
XL#COUNT DS    XL1                                                              
XL#HOFF  DS    CL2                                                              
XL#COFF  DS    CL2                 Current office                               
XL#AOFF  DS    CL2                                                              
XL#CLVL  DS    XL1                                                              
XL#SRAL  DS    XL1                                                              
XL#SRWN  DS    XL1                                                              
XL#SRW1  DS    XL1                                                              
XL#SRW2  DS    XL1                                                              
XL#SRW3  DS    XL1                                                              
XL#SRCT  DS    H                                                                
XL#SMXQ  EQU   (24*ONEK)/WORDLQ                                                 
XL#CWNO  DS    XL1                                                              
XL#APPL  DS    CL1                 APPLICATION FOR AL, AD AND SR                
XL#SVMED DS    CL1                 MEDIA CODE                                   
XL#SVINC DS    CL(L'ACTKULA)       INCOME ACCOUNT                               
XL#AGACC DS    CL(L'ACTKULA)       AGENT ACCOUNT CODE                           
XL#AGNAM DS    CL(L'NAMEREC)       AGENT ACCOUNT NAME                           
XL#AGAD1 DS    CL(L'ADRADD1)       AGENT ADDRESS LINE 1                         
XL#AGAD2 DS    CL(L'ADRADD1)       AGENT ADDRESS LINE 2                         
XL#AGAD3 DS    CL(L'ADRADD1)       AGENT ADDRESS LINE 3                         
XL#AGAD4 DS    CL(L'ADRADD1)       AGENT ADDRESS LINE 4                         
XL#APPR  DS    CL1                                                              
XL#USAP  DS    CL1                                                              
XL#LIML  DS    CL1                                                              
XL#LVL   DS    CL1                                                              
XL#TYPE  DS    XL1                 TYPE OF CALL                                 
XL#SRCH  EQU   X'01'               SEARCH                                       
XL#LIST  EQU   X'02'               LIST                                         
XL#ALL   DS    XL1                 ALL 1R list for all persons                  
X#LLIND  DS    XL1                 Limit list indicator                         
X#LLINI  EQU   X'80'               Limit list initialised                       
X#LLTABS DS    0C                  LIMIT LIST ACCESS:                           
X#LLTCPJ DS    CL1                 CLI/PRO/JOB                                  
X#LLTETY DS    CL1                 EXPENDITURE TYPE                             
X#LLTMED DS    CL1                 MEDIA                                        
X#LLTSUP DS    CL1                 SUPPLIER                                     
X#LLT1R  DS    CL1                 1R ACCOUNTS                                  
X#LLTNC  DS    CL1                 NON CLIENT ACCOUNTS                          
X#LLTWC  DS    CL1                 WORKCODE                                     
X#LLTLQ  EQU   *-X#LLTABS                                                       
X#LLALLQ EQU   C'A'                * UNLIMITED ACCESS                           
X#LLLSTQ EQU   C'L'                * LIMITED BY LIST                            
X#LLNONQ EQU   C'N'                * NO ACCESS                                  
X#NPLVL  DS    XL1                 PREVIOUS LEVELS                              
*&&US                                                                           
         DS    CL56                                                             
*&&                                                                             
                                                                                
XL#JLIND DS    CL1                                                              
XL#ADR1  DS    A                                                                
XL#ADR2  DS    A                                                                
XL#ADR3  DS    A                                                                
XL#ADR4  DS    A                                                                
XL#TDPT  DS    CL12                DEPARTMENT CODE, SPACE-FILLED                
XL#MYSW  DS    XL1                                                              
XL#ULA   DS    0CL14                                                            
XL#UL    DS    0CL2                                                             
XL#UNT   DS    CL1                                                              
XL#LDG   DS    CL1                                                              
XL#ACT   DS    CL12                                                             
XL#LEN   DS    XL1                                                              
XL#COFFL DS    CL200               List of offices                              
X#PPID   DS    XL2                                                              
DVALUESL EQU   *-DVALUES                                                        
X#OBAREA DS    XL(OB_LNQ)          Buffer area (not to be cleared)              
         EJECT                                                                  
                                                                                
***********************************************************************         
* REQUEST IN/OUT DEFINITION IN STORAGE                                *         
***********************************************************************         
                                                                                
QVALUES  DS    0X                  ** REQUEST VALUES                            
RQ_DATA  DS    0XL1                - REQUEST DATA                               
RQ_ADACC DS    CL14                - ACCOUNT DETAILS ACCOUNT CODE               
RQ_ADVLL DS    CL1                 - ACCOUNT DETAILS VALIDATE LIMLIST           
RQ_ADLED DS    CL1                 - ACCOUNT DETAILS ACCOUNT TYPE               
RQ_ADVAP DS    CL1                 - ACCOUNT DETAILS spare spare                
RQ_ADAPL DS    CL1                 - ACCOUNT DETAIL APPLICATION                 
RQ_ADORD EQU   C'O'                ORDERS                                       
RQ_ADTIM EQU   C'T'                TIMESHEETS                                   
RQ_ADEST EQU   C'E'                ESTIMATES                                    
RQ_ADEJL EQU   C'B'                ESTIMATE FOR JOB LIST                        
RQ_ADEXP EQU   C'X'                EXPENSES                                     
RQ_ADJOB EQU   C'J'                JOBS                                         
RQ_ADREP EQU   C'R'                REPORTING                                    
RQ_ADINV EQU   C'I'                INVOICES                                     
RQ_ADRES EQU   C'S'                RESOURCES                                    
RQ_ADDAS EQU   C'D'                DASHBOARD                                    
RQ_MAINT EQU   C'M'                MAINTENANCE                                  
RQ_ADMED DS    CL1                 - ACCOUNT DETAILS MEDIA CODE                 
RQ_ADIDT DS    CL8                 INVOICE DATE                                 
RQ_ADISJ DS    CL14                SPARE                                        
RQ_ADIOF DS    CL2                 OFFICE                                       
RQ_ADFPT DS    CL1                 ACCOUNT DETAIL FORCE PRODUCT ON TIME         
RQ_ADFJT DS    CL1                 ACCOUNT DETAIL FORCE JOB ON TIME             
RQ_ADBIL DS    CL1                 AC DETAIL BILLABLE ENTRY FOR ORDERS          
RQ_ADFTM DS    CL1                 AC DETAIL FUTURE TIME ONLY                   
RQ_ADEPT DS    CL6                 DEPARTMENT                                   
RQ_ADWC  DS    CL2                 WORK CODE                                    
RQ_ADRLK DS    CL1                 RETRUN LOCKED ACCOUNT (T/S)                  
         ORG   QVALUES                                                          
RQ_ALIC  DS    CL1                 - APPR LIMIT LIST : CATEGORY                 
RQ_ALIS  DS    CL1                 - APPR LIMIT LIST : SUB CATEGORY             
         ORG   QVALUES                                                          
RQ_ALLDG DS    CL2                 - A/C LIST: UNIT/LEDGER                      
RQ_ALCLI DS    CL12                - A/C LIST: CLIENT FOR PRODUCT LIST          
RQ_ALPRO DS    CL12                - A/C LIST: PRODUCT FOR JOB LIST             
RQ_ALSUB DS    CL12                - A/C LIST: SUB-DEPT FOR PERSON LIST         
RQ_ALILA DS    CL1                 - A/C LIST: INCLUDE LOCKED A/CS              
RQ_ALAPL DS    CL1                 - A/C LIST: APPLICATION                      
RQ_ALORD EQU   RQ_ADORD            ORDERS                                       
RQ_ALTIM EQU   RQ_ADTIM            TIMESHEETS                                   
RQ_ALEST EQU   RQ_ADEST            ESTIMATES                                    
RQ_ALEJL EQU   RQ_ADEJL            ESTIMATE FOR JOB LIST                        
RQ_ALEXP EQU   RQ_ADEXP            EXPENSES                                     
RQ_ALJOB EQU   RQ_ADJOB            JOBS                                         
RQ_ALREP EQU   RQ_ADREP            REPORTING                                    
RQ_ALINV EQU   RQ_ADINV            INVOICES                                     
RQ_ALRES EQU   RQ_ADRES            RESOURCES                                    
RQ_ALDAS EQU   RQ_ADDAS            DASHBOARD                                    
RQ_ADYNO DS    CL1                 - A/C LIST: draft a/cs Y/N/O                 
RQ_ALVAS DS    CL1                 - A/C LIST: VIEW ALL SECURITY Y/N            
RQ_ALAPP DS    CL1                 - A/C LIST: Spare no longer in use           
RQ_ALVIO DS    CL1                 - A/C LIST: VAT a/c In or Out                
RQ_ALMED DS    CL1                 - A/C LIST: Media code filter                
RQ_ALSOR DS    CL1                 - A/C LIST: Suppr office on return           
RQ_ALRFN DS    CL1                 - A/C LIST: Return foreign names             
RQ_ALFPT DS    CL1                 - A/C LIST: Force product on time            
RQ_ALFJT DS    CL1                 - A/C LIST: Force job on time                
RQ_ALMJO DS    CL1                 - A/C LIST: Master jobs only                 
RQ_ALCLO DS    CL1                 - A/C LIST: Closed accounts                  
RQ_ALBIL DS    CL1                 - A/C LIST: Billable entry for order         
RQ_ALFTM DS    CL1                 - A/C LIST: Future time only                 
RQ_ALIDT DS    CL8                 - A/C LIST: Invoice date                     
RQ_ALWC  DS    CL2                 - A/C LIST: Work code                        
RQ_ALRLK DS    CL1                 - A/C LIST: Rtn locked account (T/S)         
         ORG   QVALUES                                                          
RQ_SRCUL DS    CL2                 - SEARCH: UNIT/LEDGER                        
RQ_SRCLV DS    CL1                 - SEARCH: LEVEL                              
RQ_SRCOF DS    CL2                 - SEARCH: OFFICE FILTER                      
RQ_SRCAC DS    CL12                - SEARCH: ACCOUNT PREFIX                     
RQ_SRCTY DS    CL1                 - SEARCH: A=AND OR O=OR SEARCH               
*&&UK                                                                           
RQ_SRCW1 DS    CL10                - SEARCH: WORD #1                            
*&&                                                                             
*&&US                                                                           
RQ_SRCW1 DS    CL20                - SEARCH: WORD #1                            
*&&                                                                             
RQ_SRCS1 DS    CL1                 - SEARCH:  - IS PREFIX Y/N                   
*&&UK                                                                           
RQ_SRCW2 DS    CL10                - SEARCH: WORD #2                            
*&&                                                                             
*&&US                                                                           
RQ_SRCW2 DS    CL20                - SEARCH: WORD #2                            
*&&                                                                             
RQ_SRCS2 DS    CL1                 - SEARCH:                                    
*&&UK                                                                           
RQ_SRCW3 DS    CL10                - SEARCH: WORD #3                            
*&&                                                                             
*&&US                                                                           
RQ_SRCW3 DS    CL20                - SEARCH: WORD #3                            
*&&                                                                             
RQ_SRCS3 DS    CL1                 - SEARCH:                                    
RQ_SRCSF DS    CL1                 - SEARCH: SECURITY FLAG Y/N                  
RQ_SRAPL DS    CL1                 - SEARCH: APPLICATION                        
RQ_SRORD EQU   RQ_ADORD            ORDERS                                       
RQ_SRTIM EQU   RQ_ADTIM            TIMESHEETS                                   
RQ_SREST EQU   RQ_ADEST            ESTIMATES                                    
RQ_SREJL EQU   RQ_ADEJL            ESTIMATES FOR JOB LIST                       
RQ_SREXP EQU   RQ_ADEXP            EXPENSES                                     
RQ_SRJOB EQU   RQ_ADJOB            JOBS                                         
RQ_SRREP EQU   RQ_ADREP            REPORTING                                    
RQ_SRINV EQU   RQ_ADINV            INVOICES                                     
RQ_SRRES EQU   RQ_ADRES            RESOURCES                                    
RQ_SRDAS EQU   RQ_ADDAS            DASHBOARD                                    
RQ_SRYNO DS    CL1                 - SEARCH: draft a/c Y/N/O                    
RQ_SRAPP DS    CL1                 - SEARCH: spare no longer in use             
RQ_SRVIO DS    CL1                 - SEARCH: VAT a/c in or out                  
RQ_SRMED DS    CL1                 - SEARCH: media code filter                  
RQ_SRDEP DS    CL6                 - SEARCH: department filter                  
RQ_SRFPT DS    CL1                 - SEARCH: Force product on time              
RQ_SRFJT DS    CL1                 - SEARCH: Force job on time                  
RQ_SRMJO DS    CL1                 - SEARCH: Master jobs only                   
RQ_SRILA DS    CL1                 - SEARCH: Locked accounts                    
RQ_SRCLO DS    CL1                 - SEARCH: Closed accounts                    
RQ_SRBIL DS    CL1                 - SEARCH: Billable entry for order           
RQ_SRFTM DS    CL1                 - SEARCH: Future time only                   
RQ_SRIDT DS    CL8                 - SEARCH: Invoice date                       
RQ_SRWC  DS    CL2                 - SEARCH: Work code                          
RQ_SRLCK DS    CL1                 - SEARCH: Rtn locked account (T/S)           
         ORG                                                                    
QVALUESL EQU   *-QVALUES                                                        
                                                                                
OVALUES  DS    0X                                                               
         ORG   OVALUES                                                          
AD_VALS  DS    0X                  ** ACCOUNT DETAILS RETURN DATA               
AD_NAME  DS    CL36                - NAME                                       
AD_FNAM  DS    CL36                - foreign name                               
AD_ADD1  DS    CL40                - ADDRESS LINES 1-4                          
AD_ADD2  DS    CL40                                                             
AD_ADD3  DS    CL40                                                             
AD_ADD4  DS    CL40                                                             
AD_CURC  DS    CL3                 - CURRENCY CODE                              
AD_BILA  DS    CL1                 - BILLABLE ALLOWED                           
AD_CHGA  DS    CL1                 - CHARGEABLE ALLOWED                         
AD_NONA  DS    CL1                 - NON-BILLABLE ALLOWED                       
AD_DEFT  DS    CL1                 - DEFAULT TIME TYPE                          
AD_OFFC  DS    CL2                 - Office (SJ)                                
AD_DRFT  DS    CL1                 - Draft status                               
AD_ISFO  DS    CL1                 - Foreign language user                      
AD_FPT   DS    CL1                 - Force product on timesheet                 
AD_FJT   DS    CL1                 - Force job on timesheet                     
AD_BOE   DS    CL1                 - BrandOcean estiamte user (job)             
AD_FNTB  DS    CL1                 - Force narrative billable time              
AD_FNTR  DS    CL1                 - Force narrative memo (R) time              
AD_FNTN  DS    CL1                 - Force narrative non billable time          
AD_CLOS  DS    CL1                 - Close status                               
AD_VATR  DS    CL4                 - VAT rate on SG                             
AD_JOBT  DS    CL1                 - Job input allowed in time                  
AD_ANAD  DS    CL1                 - 2D ANALYSIS Y/N                            
AD_ANAP  DS    CL1                 - 2P ANALYSIS Y/N                            
AD_VATC  DS    CL5                 - VAT CODE                                   
AD_KSVR  DS    CL4                 - KSV rate                                   
AD_DISC  DS    CL4                 - Discount                                   
AD_DDXP  DS    CL8                 DUE DATE FORMULA                             
AD_DLOCB DS    CL1                 LOCKED FROM BILLING                          
AD_OFFN  DS    CL36                OFFICE NAME (FOR SJ)                         
AD_NJLE  DS    CL1                 NO JOB LEVEL ENTRY                           
AD_CSTG  DS    CL1                 COSTING GROUP                                
AD_BILO  DS    CL1                 BILLABLE ONLY                                
AD_MJO   DS    CL1                 MASTER JOB OR NOT                            
AD_LOC   DS    CL1                 LOCKED Y/N/O                                 
AD_JLE   DS    CL1                 JOB LOCKS ESTIMATES                          
AD_JLO   DS    CL1                 JOB LOCKS ORDERS                             
AD_JLT   DS    CL1                 JOB LOCKS TIMESHEETS                         
AD_JLA   DS    CL1                 JOB LOCKS ADJUSTMENTS                        
AD_JLX   DS    CL1                 JOB LOCKS EXTERNALS                          
AD_MJC   DS    CL12                MASTER JOB CODE IF SUB JOB                   
AD_CLOF  DS    XL2                 CLOSE DATE OFFSET                            
AD_AGAC  DS    CL14                AGENT ACCOUNT CODE                           
AD_AGNAM DS    CL36                AGENT NAME                                   
AD_AGAD1 DS    CL26                AGENT ADDRESS LINES 1-4                      
AD_AGAD2 DS    CL26                                                             
AD_AGAD3 DS    CL26                                                             
AD_AGAD4 DS    CL26                                                             
AD_FUTA  DS    CL1                 FUTURE TIME ALLWOED?                         
AD_MDOWC DS    CL1                 MEDIA OR WORKCODE                            
AD_MINC  DS    CL14                MEDIA INCOME/SUSPENSE ACCOUNT                
AD_NAE   DS    CL1                 NEED APPROVED ESTIMATE                       
AD_13BVI DS    CL1                 13B VAT?                                     
AD_BKSV  DS    CL1                 KSV IS BILLABLE?                             
AD_ANAM  DS    CL1                 - MILEAGE ANALYSIS Y/N                       
AD_GAP   DS    CL1                 GAP SERVICE IN USE?                          
AD_DFTEX DS    PL2                 DEFAULT EXPIRATION PERIOD                    
AD_DFTND DS    PL2                 DEFAULT NO. OF DAYS TO VIEW ESTIMATE         
AD_GAPEX DS    CL1                 GAP ESTIMATES EMAIL EXTENSION                
AD_GAPED DS    CL1                 GAP ESTIMATES EMAIL DISCLAIMER               
AD_GAPAR DS    CL1                 GAP APPROVAL REQUIRED FOR ALL                
AD_EMADD DS    CL50                SUPPLIER EMAIL ADDRESS                       
AD_SGAP  DS    CL1                 GAP SERVICE IN USE (SUPPLIER)?               
AD_AQAP  DS    CL1                 ACKNOWLEDGE/QUERY (SUPPLIER)?                
AD_BILTY DS    CL1                 OPT MAINT BILLING TYPE                       
AD_SWPD  DS    XL1                 SUPPRESS W/C PRINT DEFAULT                   
AD_FAX   DS    CL26                FAX NUMBER                                   
AD_TELE  DS    CL26                TELEPHONE                                    
AD_XAPP  DS    CL1                 EXPENSES APPROVER CHOSEN                     
AD_XMIL  DS    CL1                 EXPENSES MILEAGE RATES IN USE                
AD_1ROFF DS    CL(L'LOCOFF)        1R OFFICE CODE                               
AD_1RDPT DS    CL(L'LOCDEPT)       1R DEPARMENT CODE                            
AD_1RSUB DS    CL(L'LOCSUB)        1R SUB-DEPARTMENT CODE                       
AD_CCWC  DS    CL1                 COLOUR CODED WORKCODES                       
AD_WCF   DS    CL1                 WORK CODE FLAG FOR ESTIMATE CHECK            
AD_PPOR  DS    CL1                 Part approved order resubmit                 
AD_FPOR  DS    CL1                 Fully approved order resubmit                
AD_MPOR  DS    CL1                 Matched order resubmit                       
AD_VALQ  EQU   *-AD_VALS                                                        
         ORG   OVALUES                                                          
AC_VALS  DS    0X                  ** ACCOUNT LIST RETURN DATA                  
AC_ACT   DS    0CL12               - FOR SG: ACCOUNT ONLY                       
AC_ULA   DS    CL14                - U/L/ACCOUNT                                
AC_NAM   DS    CL36                - NAME                                       
AC_LOC   DS    CL1                 - LOCKED STATUS                              
AC_CLO   DS    CL1                 - CLOSED STATUS                              
AC_VAT   DS    CL4                 - VAT RATE ON SG                             
AC_DRA   DS    CL1                 - DRAFT STATUS                               
AC_FPT   DS    CL1                 - FORCE PRODUCT ON TIMESHEET                 
AC_FJT   DS    CL1                 - FORCE JOB ON TIMESHEET                     
AC_BILA  DS    CL1                 - BILLABLE TIME ALLOWED                      
AC_CHGA  DS    CL1                 - CHARGEABLE TIME ALLOWED                    
AC_NBLA  DS    CL1                 - NON BILLABLE TIME ALLOWED                  
AC_DEFT  DS    CL1                 - DEFAULT TIME                               
AC_OFFC  DS    CL2                 - OFFICE CODE ON SJ                          
AC_FNTB  DS    CL1                 - FORCE NARRATIVE ON BILL TIME               
AC_FNTR  DS    CL1                 - FORCE NARRATIVE ON MEMO TIME               
AC_FNTN  DS    CL1                 - FORCE NARRATIVE ON NON BILL TIME           
AC_JOBT  DS    CL1                 - JOB INPUT ALLOWED ON TIME                  
AC_FNAM  DS    CL36                - FOREIGN NAME                               
AC_ANAD  DS    CL1                 - 2D ANALYSIS Y/N                            
AC_ANAP  DS    CL1                 - 2P ANALYSIS Y/N                            
AC_DLOCB DS    CL1                 - LOCKED FROM BILLING                        
AC_OFFN  DS    CL36                - OFFICE NAME ON SJ                          
AC_NJLE  DS    CL1                 - NO JOB LEVEL ENTRY                         
AC_CSTG  DS    CL1                 - COSTING GROUP                              
AC_BILO  DS    CL1                 - BILLABLE ONLY                              
AC_MJO   DS    CL1                 - MASTER JOB                                 
AC_DISC  DS    CL1                 - SUPPLIER DISCOUNT RATE                     
AC_MJC   DS    CL12                - MASTER JOB CODE IF SUB JOB                 
AC_FUTA  DS    CL1                 - FUTURE TIME ALLOWED FOR 1N ACC             
AC_MDOWC DS    CL1                 MEDIA OR WORKCODE                            
AC_MINC  DS    CL14                MEDIA INCOME/SUSPENSE ACCOUNT                
AC_ADDR1 DS    CL40                ACCOUNT ADDRESS LINE 1                       
AC_ADDR2 DS    CL40                ACCOUNT ADDRESS LINE 2                       
AC_ADDR3 DS    CL40                ACCOUNT ADDRESS LINE 3                       
AC_ADDR4 DS    CL40                ACCOUNT ADDRESS LINE 4                       
AC_AGACC DS    CL14                AGENT ACCOUNT CODE                           
AC_AGNAM DS    CL36                AGENT ACCOUNT NAME                           
AC_AGAD1 DS    CL26                AGENT ACCOUNT ADDRESS LINE 1                 
AC_AGAD2 DS    CL26                AGENT ACCOUNT ADDRESS LINE 2                 
AC_AGAD3 DS    CL26                AGENT ACCOUNT ADDRESS LINE 3                 
AC_AGAD4 DS    CL26                AGENT ACCOUNT ADDRESS LINE 4                 
AC_CUR   DS    CL3                 CURRENCY CODE                                
AC_NAE   DS    CL1                 NEED APPROVED ESTIMATE                       
AC_VATC  DS    CL5                 - VAT CODE                                   
AC_KSVR  DS    CL4                 - KSV rate                                   
AC_13BVI DS    CL1                 13B VAT?                                     
AC_BKSV  DS    CL1                 BILLABLE KSV                                 
AC_ANAM  DS    CL1                 ANALYSIS FOR MILEAGE                         
AC_ISFO  DS    CL1                 FOREIGN LANGUAGE USER                        
AC_DDXP  DS    CL8                 DUE DATE FORMULA                             
AC_GAP   DS    CL1                 GAP SERVICE IN USE?                          
AC_DFTEX DS    PL2                 DEFAULT EXPIRATION PERIOD                    
AC_DFTND DS    PL2                 DEFAULT NO. OF DAYS TO VIEW ESTIMATE         
AC_GAPEX DS    CL1                 GAP ESTIMATES EMAIL EXTENSION                
AC_GAPED DS    CL1                 GAP ESTIMATES EMAIL DISCLAIMER               
AC_GAPAR DS    CL1                 GAP APPROVAL REQUIRED FOR ALL                
AC_EMADD DS    CL50                SUPPLIER EMAIL ADDRESS                       
AC_SGAP  DS    CL1                 GAP SERVICE IN USE (SUPPLIER)?               
AC_AQAP  DS    CL1                 ACKNOWLEDGE/QUERY (SUPPLIER)?                
AC_BILTY DS    CL1                 OPT MAINT BILLING TYPE                       
AC_SWPD  DS    XL1                 SUPPRESS W/C PRINTING DEFAULT                
AC_FAX   DS    CL26                FAX NUMBER                                   
AC_TELE  DS    CL26                TELEPHONE                                    
AC_XAPP  DS    CL1                 EXPENSES APPROVER CHOSEN                     
AC_XMIL  DS    CL1                 EXPENSES MILEAGE RATES IN USE                
AC_1ROFF DS    CL(L'LOCOFF)        1R OFFICE CODE                               
AC_1RDPT DS    CL(L'LOCDEPT)       1R DEPARTMENT CODE                           
AC_1RSUB DS    CL(L'LOCSUB)        1R SUB DEPARTMENT CODE                       
AC_CCWC  DS    CL1                 COLOUR CODED WORK CODES                      
AC_WCF   DS    CL1                 WORKCODE FLAG FOR ESTIMATE CHECK             
AC_PPOR  DS    CL1                 Part approved order resubmit                 
AC_FPOR  DS    CL1                 Fully approved order resubmit                
AC_MPOR  DS    CL1                 Matched order resubmit                       
AC_LNQ   EQU   *-AC_VALS                                                        
         ORG   OVALUES                                                          
SR_VALS  DS    0X                  ** SEARCH LIST RETURN DATA                   
SR_ACT   DS    0CL12               - NO UL IF SG                                
SR_ULA   DS    CL14                - UNIT/LEDGER/ACCOUNT CODE                   
SR_NAM   DS    CL36                - NAME                                       
SR_AGA   DS    CL14                - AGENT ACCOUNT CODE ON ST                   
SR_AGN   DS    CL36                - AGENT NAME ON ST                           
SR_VAT   DS    CL4                 - VAT RATE ON SG                             
SR_DRA   DS    CL1                 - DRAFT STATUS                               
SR_FPT   DS    CL1                 - FORCE PRODUCT ON TIMESHEET                 
SR_FJT   DS    CL1                 - FORCE JOB ON TIMESHEET                     
SR_BILA  DS    CL1                 - BILLABLE TIME ALLOWED                      
SR_CHGA  DS    CL1                 - CHARGEABLE TIME ALLOWED                    
SR_NBLA  DS    CL1                 - NON BILLABLE TIME ALLOWED                  
SR_DEFT  DS    CL1                 - DEFAULT TIME                               
SR_FNTB  DS    CL1                 - FORCE NARRATIVE ON BILL TIME               
SR_FNTR  DS    CL1                 - FORCE NARRATIVE ON MEMO TIME               
SR_FNTN  DS    CL1                 - FORCE NARRATIVE ON NON BILL TIME           
SR_OFFC  DS    CL2                 - OFFICE CODE (SJ)                           
SR_JOBT  DS    CL1                 - JOB INPUT ALLOWED IN TIME                  
SR_ANAD  DS    CL1                 - 2D ANALYSIS Y/N                            
SR_ANAP  DS    CL1                 - 2P ANALYSIS Y/N                            
SR_DLOCB DS    CL1                 - LOCKED FROM BILLING Y/N                    
SR_OFFN  DS    CL36                - OFFICE NAME (SJ)                           
SR_CLI   DS    CL6                 - CLIENT CODE (SJ ONLY)                      
SR_CLINM DS    CL36                - CLIENT NAME (SJ ONLY)                      
SR_PRO   DS    CL6                 - PRODUCT NAME (SJ ONLY)                     
SR_PRONM DS    CL36                - PRODUCT NAME (SJ ONLY)                     
SR_NJLE  DS    CL1                 - NO JOB LEVEL ENTRY                         
SR_CSTG  DS    CL1                 - COSTING GROUP                              
SR_BILO  DS    CL1                 - BILLABLE ONLY                              
SR_MJO   DS    CL1                 - MASTER JOB Y/N/O                           
SR_LOC   DS    CL1                 - LOCKED Y/N/O                               
SR_CLO   DS    CL1                 - CLOSED Y/N/O                               
SR_DISC  DS    CL4                 - SUPPLIER DISCOUNT RATE                     
SR_1ROFC DS    CL2                 - OFFICE CODE (1R ONLY)                      
SR_1ROFN DS    CL36                - OFFICE NAME (1R ONLY)                      
SR_1RDEC DS    CL2                 - DEPARTMENT CODE (1R ONLY)                  
SR_1RDEN DS    CL36                - DEPARTMENT NAME (1R ONLY)                  
SR_1RSDC DS    CL2                 - SUB DEPARTMENT CODE (1R ONLY)              
SR_1RSDN DS    CL36                - SUB DEPARTMENT NAME (1R ONLY)              
SR_MJC   DS    CL12                MASTER JOB CODE IF SUB JOB                   
SR_FUTA  DS    CL1                 FUTURE TIME ALLOWED?                         
SR_MDOWC DS    CL1                 MEDIA OR WORKCODE                            
SR_MINC  DS    CL14                MEDIA INCOME/SUSPENSE ACCOUNT                
SR_ADDR1 DS    CL40                ACCOUNT ADDRESS LINE 1                       
SR_ADDR2 DS    CL40                ACCOUNT ADDRESS LINE 2                       
SR_ADDR3 DS    CL40                ACCOUNT ADDRESS LINE 3                       
SR_ADDR4 DS    CL40                ACCOUNT ADDRESS LINE 4                       
SR_AGAD1 DS    CL26                AGENT ACCOUNT ADDRESS LINE 1                 
SR_AGAD2 DS    CL26                AGENT ACCOUNT ADDRESS LINE 2                 
SR_AGAD3 DS    CL26                AGENT ACCOUNT ADDRESS LINE 3                 
SR_AGAD4 DS    CL26                AGENT ACCOUNT ADDRESS LINE 4                 
SR_CUR   DS    CL3                 CURRENCY CODE                                
SR_NAE   DS    CL1                 NEED APPROVED ESTIMATE                       
SR_VATC  DS    CL5                 - VAT CODE                                   
SR_KSVR  DS    CL4                 - KSV rate                                   
SR_13BVI DS    CL1                 13B VAT?                                     
SR_BKSV  DS    CL1                 BILLABLE KSV                                 
SR_ANAM  DS    CL1                 ANALYSIS FOR MILEAGE                         
SR_ISFO  DS    CL1                 FOREIGN LANGUAGE USER                        
SR_DDXP  DS    CL8                 DUE DATE FORMULA                             
SR_FNAM  DS    CL36                FOREIGN NAME                                 
SR_GAP   DS    CL1                 GAP SERVICE IN USE?                          
SR_DFTEX DS    PL2                 DEFAULT EXPIRATION PERIOD                    
SR_DFTND DS    PL2                 DEFAULT NO. OF DAYS TO VIEW ESTIMATE         
SR_GAPEX DS    CL1                 GAP ESTIMATES EMAIL EXTENSION                
SR_GAPED DS    CL1                 GAP ESTIMATES EMAIL DISCLAIMER               
SR_GAPAR DS    CL1                 GAP APPROVAL REQUIRED FOR ALL                
SR_EMADD DS    CL50                SUPPLIER EMAIL ADDRESS                       
SR_SGAP  DS    CL1                 GAP SERVICE IN USE (SUPPLIER)?               
SR_AQAP  DS    CL1                 ACKNOWLEDGE/QUERY (SUPPLIER)?                
SR_BILTY DS    CL1                 OPT MAINT BILLING TYPE                       
SR_SWPD  DS    XL1                 SUPPRESS W/C PRINT DEFAULT                   
SR_FAX   DS    CL26                FAX NUMBER                                   
SR_TELE  DS    CL26                TELEPHONE                                    
SR_XAPP  DS    CL1                 EXPENSES APPROVER CHOSEN                     
SR_XMIL  DS    CL1                 EXPENSES MILEAGE RATES IN USE                
SR_CCWC  DS    CL1                 COLOUR CODED WORK CODES                      
SR_WCF   DS    CL1                 USE WC FLAG FOR ESTIMATE CHECK               
SR_PPOR  DS    CL1                 Part approved order resubmit                 
SR_FPOR  DS    CL1                 Fully approved order resubmit                
SR_MPOR  DS    CL1                 Matched order resubmit                       
SR_LNQ   EQU   *-SR_VALS                                                        
         ORG                                                                    
LIACBLK  DS    XL(3*ONEK)          BLOCK FOR LIMIT LIST                         
                                                                                
OVALUESL EQU   *-OVALUES                                                        
SAVEL    EQU   *-SAVED                                                          
         ORG   SAVED+(L'SVSERVER-(*-SAVED))  Online SAVED overflow trap         
         EJECT                                                                  
                                                                                
***********************************************************************         
* LOCAL DSECTS                                                        *         
***********************************************************************         
                                                                                
WORDTD   DSECT                                                                  
WORDAC   DS    CL12                account code                                 
WORDDA   DS    XL4                 disk address                                 
WORDWS   DS    XL1                 Word status                                  
WORD1Q   EQU   X'08'                                                            
WORD2Q   EQU   X'04'                                                            
WORD3Q   EQU   X'01'                                                            
WORDOFF  DS    XL2                 SJ office                                    
WORDLQ   EQU   *-WORDTD            length of entry                              
*                                                                               
OB_D     DSECT                                                                  
                                                                                
OB_KEY   DS    XL42                Record key                                   
                                                                                
OB_ERROR DS    XL2                 Error number (Zero=OK)                       
                                                                                
OB_DATA  DS    0X                  Data starts here                             
OB_NAME  DS    CL(L'NAMEREC)       Record name                                  
OB_OTHER DS    0X                  Other data                                   
         ORG   OB_D+256                                                         
                                                                                
OB_DATAL EQU   *-OB_ERROR                                                       
OB_LNQ   EQU   *-OB_D                                                           
                                                                                
         PRINT OFF                                                              
*PREFIX=X_                                                                      
       ++INCLUDE DDDPRINTL                                                      
*PREFIX=                                                                        
         ORG   X_P                                                              
PLINEL   DS    0CL(L'X_P)           ** DATAMGR trace line **                    
PDESC    DS    CL50                                                             
                                                                                
       ++INCLUDE ACBRAWRKD                                                      
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'202ACBRA11   12/09/19'                                      
         END                                                                    
