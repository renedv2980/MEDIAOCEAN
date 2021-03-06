*          DATA SET DEDEM81    AT LEVEL 066 AS OF 08/27/14                      
*PHASE T21B81A,*                                                                
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* JAN30/00 054 BPOO - ADD DECIMAL OPTION ON MAINFRAME TO DISPLAY      *         
* May01/00 031 GLEE - Allow P+S1/P+S2 input for PAV/STATBKS requests  *         
*                                                                     *         
* Mar08/00 030 GLEE - Support for DCONTROL market table               *         
*                                                                     *         
* Mar08/00 029 GLEE - Modify for changes to DSECTs                    *         
*                                                                     *         
* Oct04/99 028 GLEE - Support for VPH in WTP file                     *         
*                                                                     *         
* Jul09/99 010 GLEE - Chg min data len for UPT & UPP optns from 5 to 4*         
*                                                                     *         
* May26/99 007 GLEE - New NWSYNBKS action                             *         
*                                                                     *         
* Mar05/99 002 GLEE - Moved some more tables from DEDEM00             *         
*                                                                     *         
* Oct28/98 001 GLEE - New PAN book containing $DEM tables             *         
*                                                                     *         
***********************************************************************         
*        TITLE 'DEDEM80 - $DEM TABLES (SYSTAB)'                                 
* TABLE OF SYSTEMS (SEE SYSTABD)                                                
*                                                                               
T21B81   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21B81**,RA,RR=R2                                              
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
PHSTABLE DC    A(T21B81)                                                        
DEM81TAB DS    0X                                                               
         DC    A(SYSTAB)                                                        
         DC    A(ACTTAB)                                                        
         DC    A(ACTHELP)                                                       
         DC    A(ACTNEXT)                                                       
         DC    A(FILTAB)                                                        
         DC    A(FMSTAB)                                                        
***      DC    A(OPTTAB)                                                        
***      DC    A(STTABADD)                                                      
***      DC    A(STTABCLR)                                                      
         DC    A(TABLEN)                                                        
         DC    A(L'TABLEN)                                                      
         DC    A(TABLENQ)                                                       
         DC    A(TABLABEL)                                                      
         DC    A(L'TABLABEL)                                                    
         DC    A(TABLABLQ)                                                      
         DC    A(UPCASETB)                                                      
SYSTAB   DS    0C                                                               
*                                  SPOTPAK VALUES                               
         DC    C'SPOTPAK ',AL1(SYSSPT),AL1(2,2)                                 
         DC    AL1(DEFAULT),C'DISPLAY '                                         
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'DDS Spotpak System'                                         
*                                  NETPAK VALUES                                
         DC    C'NETPAK  ',AL1(SYSSPT),AL1(3,3)                                 
         DC    AL1(DEFAULT),C'DISPLAY '                                         
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'DDS Netpak System'                                          
*                                  REPPAK VALUES                                
         DC    C'REPPAK  ',AL1(SYSREP),AL1(8,8)                                 
         DC    AL1(DEFAULT),C'DISPLAY '                                         
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'DDS Reppak System'                                          
*                                                                               
         DC    AL1(EOT)                                                         
         TITLE 'DEDEM80 - $DEM TABLES (ACTTAB)'                                 
* TABLE OF ACTIONS (SEE ACTTABD)                                                
*                                                                               
ACTTAB   DS    0C                                                               
*                                  GENERAL HELP                                 
ACTHELP  DC    C'HELP    ',AL1(0)                                               
         DC    AL1(HELP),AL1(1),AL1(0)                                          
         DC    AL1(0),C'   '                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'(or ?) in any field if help is needed'                      
*                                  INITIAL DOWNLOAD FOR DEM32                   
INIT     DC    C'INIT    ',AL1(STERONLY)                                        
         DC    AL1(GRID),XL1'0B',AL1(0)                                         
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(DEFAULT),C'NSI'                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'                                     '                      
*                                                                               
*                                  INITIAL DOWNLOAD FOR PROPOSER                
PINIT    DC    C'PINIT   ',AL1(STERONLY)                                        
         DC    AL1(PROPINIT),XL1'0B',AL1(0)                                     
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(DEFAULT),C'NSI'                                              
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'                                     '                      
*                                                                               
*                                  DEMO DISPLAY BY QUARTER HOUR                 
         DC    C'DISPLAY ',AL1(0)                                               
         DC    AL1(DEMOQHR),AL1(2),AL1(DBGETDEM)                                
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(MAXSTAS)                                                     
         DC    AL1(MAXBKS)                                                      
         DC    AL1(MAXDAYS)                                                     
         DC    AL1(8)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPROGB-OPTPRNTB-OPTTIMEB-OPTDTYPB-OPTDECB-X        
               OPTYSPRB-OPTNSPRB-OPTDMAB-OPTBESTB-OPTNORB-OPTWKVB-OPTCAX        
               VGB)                                                             
         DC    CL40'displays demos by quarter hour'                             
*                                  DEMO DISPLAY FOR TIME PERIOD                 
         DC    C'LILO    ',AL1(0)                                               
         DC    AL1(DEMOQHR),XL1'0C',AL1(DBGETDEM)                               
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(MAXSTAS)                                                     
         DC    AL1(MAXBKS)                                                      
         DC    AL1(MAXDAYS)                                                     
         DC    AL1(8)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPROGB-OPTPRNTB-OPTTIMEB-OPTDTYPB-OPTDECB-X        
               OPTYSPRB-OPTNSPRB-OPTDMAB-OPTBESTB-OPTNORB-OPTWKVB-OPTCAX        
               VGB)                                                             
         DC    CL40'displays LILO  STUFF          '                             
*                                  DEMO DISPLAY FOR TIME PERIOD                 
         DC    C'PERIOD  ',AL1(0)                                               
         DC    AL1(DEMOPER),AL1(2),AL1(DBGETDEM)                                
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(MAXSTAS)                                                     
         DC    AL1(MAXBKS)                                                      
         DC    AL1(MAXDAYS)                                                     
         DC    AL1(8)                                                           
         DC    AL4(0)                                                           
*&&DO                                                                           
         DC    AL4(OPTLISTB+OPTSTRTB+OPTDPRTB+OPTPTYPB+OPTDATEB+OPTSECSX        
               B+OPTPNLSB+OPTPMLSB+OPTBESTB+OPTNORB)                            
*&&                                                                             
         DC    AL4(OPTLISTB+OPTSTRTB+OPTDATEB+OPTSECSB+OPTPNLSB+OPTPMLSx        
               B+OPTBESTB+OPTNORB+OPTSVISB)                                     
         DC    CL40'displays demos for time period'                             
*                                  DEMO DISPLAY FOR TIME PERIOD (AVAIL)         
         DC    C'AVAIL   ',AL1(ACTAVAIL)                                        
         DC    AL1(DEMOAVL),AL1(2),AL1(DBGETDEM)                                
         DC    AL1(PRESET),C'TP '                                               
         DC    AL1(0),C'   '                                                    
         DC    AL1(MAXSTAS)                                                     
         DC    AL1(MAXBKS)                                                      
         DC    AL1(MAXDAYS)                                                     
         DC    AL1(8)                                                           
         DC    AL4(0)                                                           
*&&DO                                                                           
         DC    AL4(OPTLISTB+OPTSTRTB+OPTDPRTB+OPTPTYPB+OPTDATEB+OPTSECSX        
               B+OPTPNLSB+OPTPMLSB+OPTBESTB+OPTNORB+OPTWKVB)                    
*&&                                                                             
         DC    AL4(OPTLISTB+OPTSTRTB+OPTDATEB+OPTSECSB+OPTPNLSB+OPTPMLSx        
               B+OPTBESTB+OPTNORB+OPTWKVB+OPTSVISB)                             
         DC    CL40'displays demos in avail format'                             
*&&DO                                                                           
*                                  DEMO DISPLAY FOR ESTIMATE BOOK               
         DC    C'ESTIMATE',AL1(ACTAVAIL+ACTEBOOK)                               
         DC    AL1(DEMOEST),AL1(7),AL1(0)                                       
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(8)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPRNTB-OPTDPRTB-OPTPTYPB)                          
         DC    CL40'displays demos in estimate book format'                     
*                       DEMO DISPLAY FOR NEW ESTIMATE BOOK                      
         DC    C'ESTIMATE',AL1(ACTAVAIL+ACTEBOOK)                               
         DC    AL1(DEMOEST),AL1(7),AL1(0)                                       
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(4)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(8)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPRNTB-OPTDPRTB-OPTPTYPB)                          
         DC    CL40'displays demos in estimate book format'                     
*&&                                                                             
*                                  DEMO DISPLAY FOR AFFIDAVITS                  
         DC    C'AFFIDS  ',AL1(ACTAFFID)                                        
         DC    AL1(AFFIDS),AL1(8),AL1(0)                                        
         DC    AL1(DEFAULT),C'TP '                                              
*****    DC    AL1(DEFAULT+PRESET),C'   '                                       
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(6)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPRNTB-OPTDATEB-OPTSVISB-OPTSECSB-OPTDMAB-X        
               OPTCAVGB-OPTDECB)                                                
         DC    CL40'displays affidavit data for a buy line'                     
*                                  LIST ACTIVE MARKETS FOR A BOOK               
         DC    C'MARKET  ',AL1(0)                                               
         DC    AL1(MARKETS),AL1(4),AL1(DBGETMKB)                                
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTLISTB-OPTPRNTB-OPTSTRTB)                          
         DC    CL40'lists active markets for a book'                            
*                                  LIST STATIONS FOR A MARKET                   
         DC    C'STATION ',AL1(0)                                               
         DC    AL1(STATION),AL1(3),AL1(DBGETMS)                                 
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPRNTB)                                            
         DC    CL40'lists active stations for a market'                         
*                                  LIST ACTIVE BOOKS FOR A STATION              
         DC    C'BOOK    ',AL1(0)                                               
         DC    AL1(STATBKS),AL1(6),AL1(DBGETMB)                                 
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTLISTB-OPTPRNTB)                                   
         DC    CL40'lists available books for a station'                        
*                                  LIST ACTIVE BOOKS FOR NTWK/SYN PGMS          
         DC    C'BOOKP   ',AL1(0)                                               
         DC    AL1(NWSYNBKS),AL1(6),AL1(DBGETMB)                                
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTLISTB-OPTPRNTB)                                   
         DC    CL40'lists books containing netwk/synd pgms'                     
*                                  LIST STATION SPILL MARKETS                   
         DC    C'SPILL   ',AL1(0)                                               
         DC    AL1(SPILLTO),AL1(3),AL1(DBGETSM)                                 
         DC    AL1(DEFAULT),C'TP '                                              
*^^NOP   DC    AL1(PRESET),C'TP '                                               
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPRNTB)                                            
         DC    CL40'lists markets for a station'                                
*                                  LIST VALID DEMOS FOR A BOOK                  
         DC    C'DEMO    ',AL1(0)                                               
         DC    AL1(DEMOLST),AL1(5),AL1(0)                                       
         DC    AL1(0),C'   '                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPRNTB)                                            
         DC    CL40'lists valid demo codes for a book'                          
*                                  LIST PROGRAMS FOR A BOOK                     
         DC    C'PROGNAME',AL1(0)                                               
         DC    AL1(PROGNAME),AL1(9),AL1(DBGETIPR)                               
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPNLSB-OPTPRNTB)                                   
         DC    CL40'lists netwk/synd programs for a book'                       
*                                  LIST PROGRAM NUMBERS FOR A BOOK              
         DC    C'PROGLIST',AL1(0)                                               
         DC    AL1(PROGLIST),AL1(9),AL1(DBGETISI)                               
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPRNTB)                                            
         DC    CL40'LISTS NETWK/SYND PROGRAM #S FOR A BOOK'                     
*                                  LIST MKTS/STTNS FOR A PROGRAM                
         DC    C'PROGMKT ',AL1(0)                                               
         DC    AL1(PROGMKT),XL1'0A',AL1(0)                                      
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPMLSB-OPTPRNTB-OPTDMAB-OPTCAVGB-OPTDECB)          
         DC    CL40'lists mkts/sttns which ran a program'                       
*                                                                               
         DC    C'PROGMRNK',AL1(0)                                               
         DC    AL1(PROGMKT),XL1'0A',AL1(0)                                      
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPMLSB-OPTPRNTB-OPTDMAB-OPTCAVGB)                  
         DC    CL40'lists mkts/sttns which ran a program'                       
*                                  NEXT                                         
ACTNEXT  DC    C'NEXT    ',AL1(0)                                               
         DC    AL1(NEXT),AL1(0),AL1(NEXT)                                       
         DC    AL1(0),C'   '                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'when there is more data to display'                         
*                                  FIRST                                        
ACTFRST  DC    C'FIRST   ',AL1(0)                                               
         DC    AL1(NEXT),AL1(0),AL1(FRST)                                       
         DC    AL1(0),C'   '                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'to restart display at first screen'                         
*                                                                               
*                                  LIST SYSCODES FOR A MARKET                   
         DC    C'SYSCODE ',AL1(0)                                               
         DC    AL1(SYSCODE),AL1(3),AL1(DBGETSYS)                                
         DC    AL1(DEFAULT),C'TP '                                              
         DC    AL1(0),C'   '                                                    
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPRNTB-OPTLISTB-OPTSTRTB)                          
         DC    CL40'LISTS SYSCODES FOR A MARKET'                                
         DC    AL1(EOT)                                                         
         TITLE 'DEDEM80 - $DEM TABLES (FILTAB)'                                 
*                                                                               
* TABLE OF FILES (SEE FILTABD)                                                  
*                                                                               
FILTAB   DS    0C                                                               
*                                  TIME PERIOD VALUES/MARKETS                   
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                                  RADIO DAYPARTS VALUES/MARKETS                
         DC    C'RDP',AL1(DDSONLY)                                              
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                                WEEKLY PERIOD VALUES/MARKETS                   
         DC    C'WTP',AL1(0)                                                    
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                  LOCAL METER   WEEKLY PERIOD VALUES/MARKETS                   
         DC    C'LPM',AL1(0)                                                    
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                                RADIO  PERIOD VALUES/MARKETS                   
         DC    C'RTP',AL1(0)                                                    
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                                  PROGRAM AVERAGE VALUES/MARKETS               
         DC    C'PAV',AL1(STERONLY)                                             
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                                                                               
*                                COUNTY COVERAGE/MARKETS                        
         DC    C'CTP',AL1(0)                                                    
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                                                                               
*                                OVERNIGHTS/MARKETS                             
         DC    C'OTP',AL1(0)                                                    
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                                                                               
*                           PAV  OVERNIGHTS/MARKETS                             
         DC    C'OPA',AL1(STERONLY)                                             
         DC    AL1(MARKETS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'market list file'                                           
*                                                                               
*                                  TIME PERIOD VALUES/SPILLTO                   
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(SPILLTO)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/market spill file'                                  
*                                                                               
*                                  TIME PERIOD VALUES/SPILLTO                   
         DC    C'WTP',AL1(0)                                                    
         DC    AL1(SPILLTO)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/market spill file'                                  
*                                                                               
*            LOCAL PEOPLE METER    TIME PERIOD VALUES/SPILLTO                   
         DC    C'LPM',AL1(0)                                                    
         DC    AL1(SPILLTO)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/market spill file'                                  
*                                                                               
*                                  RADIO TIME PERIOD VALUES/SPILLTO             
         DC    C'RTP',AL1(0)                                                    
         DC    AL1(SPILLTO)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/market spill file for radio'                        
*                                  PROGRAM AVERAGE VALUES/SPILLTO               
         DC    C'PAV',AL1(STERONLY)                                             
         DC    AL1(SPILLTO)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/market spill file'                                  
*                                                                               
*                                  COUNTY COVERAGE/SPILLTO                      
         DC    C'CTP',AL1(0)                                                    
         DC    AL1(SPILLTO)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/market spill file'                                  
*                                                                               
*                                  OVERNIGHT/SPILLTO                            
         DC    C'OTP',AL1(0)                                                    
         DC    AL1(SPILLTO)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/market spill file'                                  
*                                                                               
*                             PAV  OVERNIGHT/SPILLTO                            
         DC    C'OPA',AL1(STERONLY)                                             
         DC    AL1(SPILLTO)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/market spill file'                                  
*                                                                               
*                                  TIME PERIOD VALUES/STATION                   
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*                                  RADIO DAYPARTS VALUES/STATION                
         DC    C'RDP',AL1(DDSONLY)                                              
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*                                WEEKLY VALUES/STATION                          
         DC    C'WTP',AL1(0)                                                    
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*          LOCAL PEOPLE METER    WEEKLY VALUES/STATION                          
         DC    C'LPM',AL1(0)                                                    
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*                                RADIO TIME-PERIOD/STATION                      
         DC    C'RTP',AL1(0)                                                    
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*                                  PROGRAM AVERAGE VALUES/STATION               
         DC    C'PAV',AL1(STERONLY)                                             
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*                                                                               
*                                COUNTY COVERAGE/STATION                        
         DC    C'CTP',AL1(0)                                                    
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*                                                                               
*                                OVERNIGHT/STATION                              
         DC    C'OTP',AL1(0)                                                    
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*                                                                               
*                            PAV OVERNIGHT/STATION                              
         DC    C'OPA',AL1(STERONLY)                                             
         DC    AL1(STATION)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS)                                             
         DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station list file'                                          
*                                                                               
*                                  TIME PERIOD VALUES/STATBKS                   
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVSPIL)                                    
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file'                                     
*                                  RADIO DAYPARTS VALUES/STATBKS                
         DC    C'RDP',AL1(DDSONLY)                                              
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVSPIL)                                    
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file'                                     
*                                  TIME PERIOD VALUES/STATBKS                   
         DC    C'WTP',AL1(0)                                                    
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVSPIL)                                    
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file'                                     
*          LOCAL PEOPLE METER      TIME PERIOD VALUES/STATBKS                   
         DC    C'LPM',AL1(0)                                                    
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVSPIL)                                    
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file'                                     
*                                  RADIO TIME PERIOD VALUES/STATBKS             
         DC    C'RTP',AL1(0)                                                    
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVSPIL)                                    
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file for radio'                           
*                                  PROGRAM AVERAGE VALUES/STATBKS               
         DC    C'PAV',AL1(STERONLY)                                             
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVPARS+STAVSPIL)                           
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file'                                     
*                                                                               
*                                  COUNTY COVERAGE/STATBKS                      
         DC    C'CTP',AL1(0)                                                    
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVSPIL)                                    
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file for County'                          
*                                                                               
*                                  OVERNIGHTS/STATBKS                           
         DC    C'OTP',AL1(0)                                                    
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVSPIL)                                    
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file for County'                          
*                                                                               
*                              PAV OVERNIGHTS/STATBKS                           
         DC    C'OPA',AL1(STERONLY)                                             
         DC    AL1(STATBKS)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS+STAVSPIL)                                    
         DC    AL1(PRESET+STV2OPTL)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'station/book list file for County'                          
*                                                                               
*                                  TIME PERIOD VALUES/NETWK-SYND BOOKS          
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(NWSYNBKS)                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'programs/book list file'                                    
*                                  TIME PERIOD VALUES/NETWK-SYND BOOKS          
         DC    C'PAV',AL1(STERONLY)                                             
         DC    AL1(NWSYNBKS)                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'programs/book list file'                                    
*&&DO                                                                           
*                                  TIME PERIOD VALUES/ESTIMATE                  
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(DEMOEST)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVESTS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(PRESET+DEMVESTS)                                             
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period estimated demo file'                            
*&&                                                                             
*                                  TIME PERIOD VALUES/AFFID                     
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(AFFIDS)                                                      
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVBUYD)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(BKVMONTH+BKVLATST)                                           
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(PRESET+DEMVESTS)                                             
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period demographics file'                              
*                                  TIME PERIOD VALUES/AFFID                     
         DC    C'WTP',AL1(0)                                                    
         DC    AL1(AFFIDS)                                                      
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVBUYD)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(BKVMONTH+BKVLATST)                                           
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(PRESET+DEMVESTS)                                             
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period demographics file'                              
*         LOCAL PEOPLE METER       TIME PERIOD VALUES/AFFID                     
         DC    C'LPM',AL1(0)                                                    
         DC    AL1(AFFIDS)                                                      
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVBUYD)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(BKVMONTH+BKVLATST)                                           
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(PRESET+DEMVESTS)                                             
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period demographics file'                              
*                                  RADIO TIME PERIOD VALUES/AFFID               
         DC    C'RTP',AL1(0)                                                    
         DC    AL1(AFFIDS)                                                      
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVBUYD)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(BKVMONTH+BKVLATST)                                           
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(PRESET+DEMVESTS)                                             
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'radio time-period demographics file'                        
*                                  TIME PERIOD VALUES/PROGNAME                  
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(PROGNAME)                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'program name list file'                                     
*                                  PROGRAM AVERAGE VALUES/PROGNAME              
         DC    C'PAV',AL1(0)                                                    
         DC    AL1(PROGNAME)                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'program name list file'                                     
*                                  TIME PERIOD VALUES/PROGLIST                  
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(PROGLIST)                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'PROGRAM NUMBERS LIST FILE'                                  
*                                  PROGRAM AVERAGE VALUES/PROGLIST              
         DC    C'PAV',AL1(0)                                                    
         DC    AL1(PROGLIST)                                                    
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVSTAS)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'PROGRAM NUMBERS LIST FILE'                                  
*                                  TIME PERIOD VALUES/PROGMKT                   
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(PROGMKT)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+STV2NMRC)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(DEMVOPTL)                                                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'mkts/sttns for a program list file'                         
*                                  PROGRAM AVERAGE VALUES/PROGMKT               
         DC    C'PAV',AL1(0)                                                    
         DC    AL1(PROGMKT)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET)                                                      
         DC    AL1(PRESET+STV2NMRC)                                             
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(DEMVOPTL)                                                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'mkts/sttns for a program list file'                         
                                                                                
*                                  TIME PERIOD VALUES/DISPLAY                   
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB)                                            
         DC    CL40'time period demographics file'                              
*                                  WEEKLY VALUES/DISPLAY                        
         DC    C'WTP',AL1(0)                                                    
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPROGB-OPTPRNTB-OPTTIMEB-OPTDTYPB-OPTDECB)         
         DC    CL40'weekly metered mkt TP demographics file'                    
*               LOCAL PEOPLE METER WEEKLY VALUES/DISPLAY                        
         DC    C'LPM',AL1(0)                                                    
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPROGB-OPTPRNTB-OPTTIMEB-OPTDTYPB-OPTDECB)         
         DC    CL40'Local People Meter Wkly demographics file'                  
*                                  RADIO DAYPARTS/DISPLAY                       
         DC    C'RDP',AL1(DDSONLY)                                              
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(BKVMONTH+BKVVALTP)                                           
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB)                                            
         DC    CL40'radio dayparts demographics file'                           
*                                  RADIO TIME-PERIOD/DISPLAY                    
         DC    C'RTP',AL1(0)                                                    
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB+OPTMBKB)                                    
         DC    CL40'radio time-period demographics file'                        
*                                  PROGRAM AVERAGE VALUES/DISPLAY               
         DC    C'PAV',AL1(0)                                                    
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVPARS+STAVSPIL)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTSVISB+OPTDTYPB)                                           
         DC    CL40'program averages demographics file'                         
                                                                                
*                                  TIME PERIOD VALUES/PERIOD                    
         DC    C'TP ',AL1(0)                                                    
         DC    AL1(DEMOPER)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB+OPTVPHB)                                    
         DC    CL40'time period demographics file'                              
*                                  WEEKLY VALUES/PERIOD                         
         DC    C'WTP',AL1(0)                                                    
         DC    AL1(DEMOPER)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPROGB-OPTPRNTB-OPTTIMEB-OPTDTYPB-OPTVPHB-+        
               OPTMBKB-OPTDECB)                                                 
         DC    CL40'weekly metered mkt TP demographics file'                    
*                                                                               
*              LOCAL PEOPLE METER  WEEKLY VALUES/PERIOD                         
         DC    C'LPM',AL1(0)                                                    
         DC    AL1(DEMOPER)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(ALLOPTS-OPTPROGB-OPTPRNTB-OPTTIMEB-OPTDTYPB-OPTDECB-+        
               OPTVPHB-OPTMBKB)                                                 
         DC    CL40'weekly metered mkt TP demographics file'                    
*                                  RADIO TIME PERIOD VALUES/PERIOD              
         DC    C'RTP',AL1(0)                                                    
         DC    AL1(DEMOPER)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB+OPTMBKB+OPTVPHB)                            
         DC    CL40'radio time-period demographics file'                        
*                                  PROGRAM AVERAGE VALUES/PERIOD                
         DC    C'PAV',AL1(STERONLY)                                             
         DC    AL1(DEMOPER)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVPARS+STAVSPIL)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTSVISB+OPTDTYPB+OPTMBKB+OPTVPHB)                           
         DC    CL40'program averages demographics file'                         
                                                                                
*                     OVERNIGHTS   PROGRAM AVERAGE VALUES/PERIOD                
         DC    C'OPA',AL1(STERONLY)                                             
         DC    AL1(DEMOPER)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVPARS+STAVSPIL)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTSVISB+OPTDTYPB+OPTMBKB+OPTVPHB)                           
         DC    CL40'program averages demographics file'                         
                                                                                
*                                  TIME PERIOD VALUES/ESTIMATE                  
*--->    DC    C'PAV',AL1(0)                                                    
*--->    DC    AL1(DEMOEST)                                                     
*--->    DC    AL1(0),C'   '                                                    
*--->    DC    AL1(PRESET+STAVESTS)                                             
*--->    DC    AL1(PRESET)                                                      
*--->    DC    AL1(0)                                                           
*--->    DC    AL1(PRESET)                                                      
*--->    DC    AL1(0)                                                           
*--->    DC    AL1(0)                                                           
*--->    DC    AL2(0)                                                           
*--->    DC    AL2(0)                                                           
*--->    DC    CL40'program averages estimated demo file'                       
*                                  TIME PERIOD VALUES                           
*                                                                               
*                                  COUNTY COVERAGE/DISPLAY                      
         DC    C'CTP',AL1(0)                                                    
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB)                                            
         DC    CL40'time period demographics file'                              
*                                                                               
*                                  OVERNIGHTS/DISPLAY                           
         DC    C'OTP',AL1(0)                                                    
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB)                                            
         DC    CL40'time period demographics file'                              
*                                                                               
*                             PAV OVERNIGHTS/DISPLAY                            
         DC    C'OPA',AL1(0)                                                    
         DC    AL1(DEMOQHR)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
****     DC    AL4(OPTBESTB+OPTNORB)                                            
         DC    AL4(OPTSVISB+OPTDTYPB)                                           
         DC    CL40'time period demographics file'                              
*                                                                               
*                                                                               
*                                  COUNTY COVERAGE/PERIOD                       
         DC    C'CTP',AL1(0)                                                    
         DC    AL1(DEMOPER)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB+OPTVPHB)                                    
         DC    CL40'time period demographics file'                              
*                                                                               
         DC    C'OTP',AL1(0)                                                    
         DC    AL1(DEMOPER)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB+OPTNORB+OPTVPHB+OPTUPGDB)                           
         DC    CL40'time period demographics file'                              
*                                                                               
         DC    C'TP ',AL1(0)       SYSCODE ACTION                               
         DC    AL1(SYSCODE)                                                     
         DC    AL1(0),C'   '                                                    
         DC    AL1(PRESET+STAVTOTS+STAVSPIL+STAVSTAS)                           
***      DC    AL1(PRESET+STV2MRKT)                                             
         DC    AL1(0)                                                           
         DC    AL1(PRESET+BKVMONTH)                                             
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'syscode list file'                                          
*                                                                               
FILTP    DC    C'TP ',AL1(0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB)                                                    
         DC    CL40'time period demographics file'                              
*                                  TIME PERIOD VALUES                           
FILPAV   DS    0C                                                               
         DC    C'PAV',AL1(0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'program averages demographics file'                         
*                           WEEKLY TIME PERIOD                                  
FILWTP   DC    C'WTP',AL1(0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period demographics file'                              
*               LOCAL PEOPLE METER                                              
FILLPM   DC    C'LPM',AL1(0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period demographics file'                              
*                                  RADIO DAYPARTS                               
         DC    C'RDP',AL1(DDSONLY)                                              
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(BKVMONTH+BKVVALTP)                                           
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'radio dayparts demographics file'                           
*                                  RADIO TIME-PERIOD                            
         DC    C'RTP',AL1(0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(OPTMBKB)                                                     
         DC    CL40'radio time-period demographics file'                        
*                                                                               
FILCTP   DC    C'CTP',AL1(0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period demographics file'                              
*                                                                               
FILOTP   DC    C'OTP',AL1(0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period demographics file'                              
         DC    AL1(EOT)                                                         
FILOPA   DC    C'OPA',AL1(0)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0),C'   '                                                    
         DC    AL1(STAVSTAS+STAVSPIL+STAVTOTS)                                  
         DC    AL1(0)                                                           
         DC    AL1(BKVLATST+BKVMONTH+BKVVALTP)                                  
         DC    AL1(PRESET)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'time period demographics file'                              
         DC    AL1(EOT)                                                         
*                                                                               
         TITLE 'DEDEM80 - $DEM TABLES (FMSTAB)'                                 
* TABLE OF FILES/SOURCES (SEE FMSTABD)                                          
*                                                                               
FMSTAB   DS    0H                                                               
*                                  TIME PERIOD/ARB                              
         DC    C'TP ',C'ARB',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP AT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen Audio time period data'                             
*                                  TIME PERIOD/MFX (MEDIAFAX)                   
         DC    C'TP ',C'MFX',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP MT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Mediafax time period data'                                  
*                                  TIME PERIOD/NSI                              
         DC    C'TP ',C'NSI',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP NT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA time period data'                               
*                                  TIME PERIOD/BBM                              
         DC    C'TP ',C'BBM',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP AC'                                                         
         DC    AL1(STAVSTAS+STAVSPIL)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'BBM Canadian time period data'                              
*                         WEEKLY   TIME PERIOD/BBM                              
         DC    C'WTP',C'BBM',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP AC'                                                         
         DC    AL1(STAVSTAS+STAVSPIL)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'BBM Canadian time period data'                              
*                                  TIME PERIOD/CSI                              
         DC    C'TP ',C'CSI',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP NC'                                                         
         DC    AL1(STAVSTAS+STAVSPIL)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen Canadian time period data'                          
*                          WEEKLY  TIME PERIOD/CSI                              
         DC    C'WTP',C'CSI',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP NC'                                                         
         DC    AL1(STAVSTAS+STAVSPIL)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen Canadian time period data'                          
*                                  TIME PERIOD/src                              
         DC    C'TP ',C'SRC',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP ST'                                                         
         DC    AL1(STAVSTAS+STAVSPIL)                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Strategy Research time period data'                         
*                                  PROGRAM AVERAGE/ARB                          
*        DC    C'PAV',C'ARB',AL1(0)                                             
*        DC    AL1(0)                                                           
*        DC    C'PAVAT'                                                         
*        DC    AL1(STAVSTAS+STAVPARS+STAVSPIL)                                  
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL1(0)                                                           
*        DC    AL2(0)                                                           
*        DC    AL2(0)                                                           
*        DC    CL40'Nielsen Audio program averages data'                        
*                                  PROGRAM AVERAGE/MFX/MARKETS                  
         DC    C'PAV',C'MFX',AL1(STERONLY)                                      
         DC    AL1(MARKETS)                                                     
         DC    C'TP MT'                                                         
         DC    AL1(STAVSTAS+STAVPARS+STAVSPIL)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Mediafax program average data-MARKET'                       
*                                  PROGRAM AVERAGE/MFX/SPILLTO                  
         DC    C'PAV',C'MFX',AL1(STERONLY)                                      
         DC    AL1(SPILLTO)                                                     
         DC    C'TP MT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Mediafax program average data-SPILL'                        
*                                  PROGRAM AVERAGE/MFX/STATION                  
         DC    C'PAV',C'MFX',AL1(STERONLY)                                      
         DC    AL1(STATION)                                                     
         DC    C'TP MT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Mediafax program average data-STATION'                      
*                                  PROGRAM AVERAGE/MFX/BOOK                     
         DC    C'PAV',C'MFX',AL1(STERONLY)                                      
         DC    AL1(STATBKS)                                                     
         DC    C'TP MT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Mediafax program average data-BOOK'                         
*                                  PROGRAM AVERAGE/MFX (MEDIAFAX)               
         DC    C'PAV',C'MFX',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'PAVMT'                                                         
         DC    AL1(STAVSTAS+STAVPARS+STAVSPIL)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Mediafax program averages data'                             
*                                  PROGRAM AVERAGE/NSI/MARKETS                  
         DC    C'PAV',C'NSI',AL1(STERONLY)                                      
         DC    AL1(MARKETS)                                                     
         DC    C'TP NT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-MARKET'                    
*                            OVERNIGHTS PAV/NSI/MARKETS                         
         DC    C'OPA',C'NSI',AL1(STERONLY)                                      
         DC    AL1(MARKETS)                                                     
         DC    C'TP NO'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-MARKET'                    
*                                  PROGRAM AVERAGE/NSI/SPILLTO                  
         DC    C'PAV',C'NSI',AL1(STERONLY)                                      
         DC    AL1(SPILLTO)                                                     
         DC    C'TP NT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-SPILL'                     
*                       OVERNIGHTS PROGRAM AVERAGE/NSI/SPILLTO                  
         DC    C'OPA',C'NSI',AL1(STERONLY)                                      
         DC    AL1(SPILLTO)                                                     
         DC    C'TP NO'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-SPILL'                     
*                                  PROGRAM AVERAGE/NSI/STATION                  
         DC    C'PAV',C'NSI',AL1(STERONLY)                                      
         DC    AL1(STATION)                                                     
         DC    C'TP NT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-STATION'                   
*                      OVERNIGHTS  PROGRAM AVERAGE/NSI/STATION                  
         DC    C'OPA',C'NSI',AL1(STERONLY)                                      
         DC    AL1(STATION)                                                     
         DC    C'TP NO'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-STATION'                   
*                                  PROGRAM AVERAGE/NSI/BOOK                     
         DC    C'PAV',C'NSI',AL1(STERONLY)                                      
         DC    AL1(STATBKS)                                                     
         DC    C'TP NT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-BOOK'                      
*                        OVERNIGHT PROGRAM AVERAGE/NSI/BOOK                     
         DC    C'OPA',C'NSI',AL1(STERONLY)                                      
         DC    AL1(STATBKS)                                                     
         DC    C'TP NO'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-BOOK'                      
*                                  PROGRAM AVERAGE/NSI/BOOKP                    
         DC    C'PAV',C'NSI',AL1(STERONLY)                                      
         DC    AL1(NWSYNBKS)                                                    
         DC    C'TP NT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-BOOKP'                     
*                      OVERNIGHTS  PROGRAM AVERAGE/NSI/BOOKP                    
         DC    C'OPA',C'NSI',AL1(STERONLY)                                      
         DC    AL1(NWSYNBKS)                                                    
         DC    C'TP NO'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-BOOKP'                     
*                                  PROGRAM AVERAGE/NSI/PROGNAME                 
         DC    C'PAV',C'NSI',AL1(STERONLY)                                      
         DC    AL1(PROGNAME)                                                    
         DC    C'TP NT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-PROGNAM'                   
*                                  PROGRAM AVERAGE/NSI/PROGLIST                 
         DC    C'PAV',C'NSI',AL1(STERONLY)                                      
         DC    AL1(PROGLIST)                                                    
         DC    C'TP NT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NIELSEN USA PROGRAM AVERAGE DATA-PROGLIS'                   
*                                  PROGRAM AVERAGE/NSI/PROGMKT                  
         DC    C'PAV',C'NSI',AL1(STERONLY)                                      
         DC    AL1(PROGMKT)                                                     
         DC    C'PAVNT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen USA program average data-PROGMKT'                   
*                                  PROGRAM AVERAGE/NSI                          
         DC    C'PAV',C'NSI',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'PAVNT'                                                         
         DC    AL1(STAVSTAS+STAVPARS+STAVSPIL)                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen program averages data'                              
*                                  TIME PERIOD/ARB                              
         DC    C'RDP',C'ARB',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'RDPAR'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen Audio radio daypart data'                           
*                                  TIME PERIOD/BBM RADIO                        
         DC    C'RDP',C'BBM',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'RDPMR'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'BBM radio summary data'                                     
*                                                                               
*                                  TIME PERIOD/NSI                              
         DC    C'RDP',C'BIR',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'RDPNR'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Birch USA radio daypart data'                               
*                                                                               
*                        WEEKLY TIME PERIOD/NSI                                 
         DC    C'WTP',C'NSI',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP NW'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen metered market weekly'                              
*           LOCAL PEOPLE METER WKLY TIME PERIOD/NSI                             
         DC    C'LPM',C'NSI',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP NW'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen metered market weekly'                              
*                                  TIME PERIOD/ARB                              
         DC    C'RTP',C'ARB',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP AR'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Nielsen Audio radio time-period data'                       
*                                                                               
         DC    C'RTP',C'RAD',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP RR'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Radar Radio Net time-period data'                           
*                                                                               
*                                                                               
         DC    C'RTP',C'RAR',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP RR'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Radar Radio Net time-period data'                           
*                                                                               
         DC    C'CTP',C'NSI',AL1(0)      COUNTY COVERAGE                        
         DC    AL1(0)                                                           
*        DC    C'TP NU'                                                         
         DC    C'CTPNU'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'County Coverage time-period data'                           
*                                                                               
         DC    C'OTP',C'NSI',AL1(0)      OVERNIGHTS                             
         DC    AL1(0)                                                           
*        DC    C'TP NO'                                                         
         DC    C'TP NO'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Overnights TIME-PERIOD DATA'                                
*                                                                               
         DC    C'OPA',C'NSI',AL1(0)  PAV OVERNIGHTS                             
         DC    AL1(0)                                                           
*        DC    C'TP NO'                                                         
         DC    C'PAVNO'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Overnights TIME-PERIOD DATA'                                
*                                                                               
         DC    C'TP ',C'FUS',AL1(0)      FUSION DATA                            
         DC    AL1(0)                                                           
*        DC    C'TP FT'                                                         
         DC    C'TP FT'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'Fusion TIME-PERIOD DATA'                                    
*                                  TIME PERIOD/NHTI                             
         DC    C'TP ',C'NHT',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP HN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NHTI network tv time-period data'                           
*                                  PROGRAM AVERAGE/NHTI/MARKETS                 
         DC    C'PAV',C'NHT',AL1(STERONLY)                                      
         DC    AL1(MARKETS)                                                     
         DC    C'PAVHN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NHTI network program average data-MARKET'                   
*                                  PROGRAM AVERAGE/NHTI/STATION                 
         DC    C'PAV',C'NHT',AL1(STERONLY)                                      
         DC    AL1(STATION)                                                     
         DC    C'PAVHN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NHTI network PAV data-STATION'                              
*                                  PAV/NHTI                                     
         DC    C'PAV',C'NHT',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'PAVHN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NHTI network tv program data'                               
*                                                                               
*                                  TIME PERIOD/CABLE                            
         DC    C'TP ',C'NTC',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP CN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NTI network cable data'                                     
*                                  PROGRAM AVERAGE/CABLE/MARKETS                
         DC    C'PAV',C'NTC',AL1(STERONLY)                                      
         DC    AL1(MARKETS)                                                     
         DC    C'PAVCN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NTI netowrk cable prg avg data-MARKET'                      
*                                  PROGRAM AVERAGE/CABLE/STATION                
         DC    C'PAV',C'NTC',AL1(STERONLY)                                      
         DC    AL1(STATION)                                                     
         DC    C'PAVCN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NTI network cable pav data-STATION'                         
*                                  PAV/NTI CABLE                                
         DC    C'PAV',C'NTC',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'PAVCN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NTI network cable program data'                             
*                                                                               
*                                  PAV/NAD                                      
         DC    C'PAV',C'NAD',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'PAVDN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NAD NETWORK TV PROGRAM DATA'                                
*                                 TP/NAD                                        
         DC    C'TP ',C'NAD',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP DN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NAD NETWORK TV PROGRAM DATA'                                
*                                                                               
*                                  TIME PERIOD/NTI                              
         DC    C'TP ',C'NTI',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'TP KN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NTI NETWORK TV TIME-PERIOD DATA'                            
*                                  PAV/NTI                                      
         DC    C'PAV',C'NTI',AL1(0)                                             
         DC    AL1(0)                                                           
         DC    C'PAVKN'                                                         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'NTI NETWORK TV PROGRAM DATA'                                
*                                                                               
         DC    AL1(EOT)                                                         
*&&DO                                                                           
         TITLE 'DEDEM80 - $DEM TABLES (OPTTAB)'                                 
* TABLE OF OPTIONS (SEE OPTTABD)                                                
OPTTAB   DS    0X                                                               
*                                  UPT=TYPE/VALUE(BOOK)/VALUE(BOOK)             
         DC    C'TUPGRADE',C'UPT',AL1(OPTARTN)                                  
         DC    AL1(2,4,30)                                                      
         DC    AL1(OPTUPGDN),AL4(OPTUPGDB)                                      
         DC    AL2(VALUPGD-DEM00,OPTUPGD-DEMWRKD),AL1(L'OPTUPGD)                
         DC    AL4(0)                                                           
         DC    AL4(OPTSVISB)                                                    
         DC    CL40'upgrades from time period data'                             
*                                  UPP=TYPE/VALUE(BOOK)/VALUE(BOOK)             
         DC    C'PUPGRADE',C'UPP',AL1(OPTARTN)                                  
         DC    AL1(3,4,30)                                                      
         DC    AL1(OPTUPGDN),AL4(OPTUPGDB)                                      
         DC    AL2(VALUPGD-DEM00,OPTUPGD-DEMWRKD),AL1(L'OPTUPGD)                
         DC    AL4(0)                                                           
         DC    AL4(OPTSVISB)                                                    
         DC    CL40'upgrades from program averages data'                        
*                                  PUTS=N                                       
         DC    C'PUTS    ',C'   ',AL1(OPTATAB)                                  
         DC    AL1(2,1,5)                                                       
         DC    AL1(OPT2YRPN),AL4(OPT2YRPB)                                      
         DC    AL2(TBLPUTS-DEM00,OPT2YRP-DEMWRKD),AL1(L'OPT2YRP)                
         DC    AL4(OPTUPGDB)                                                    
         DC    AL4(0)                                                           
         DC    CL40'specifies number of put years to look-up'                   
*                                  RATINGS=N                                    
         DC    C'RATINGS ',C'RTG',AL1(OPTATAB)                                  
         DC    AL1(2,1,5)                                                       
         DC    AL1(OPT2YRRN),AL4(OPT2YRRB)                                      
         DC    AL2(TBLPUTS-DEM00,OPT2YRR-DEMWRKD),AL1(L'OPT2YRR)                
         DC    AL4(OPTUPGDB)                                                    
         DC    AL4(0)                                                           
         DC    CL40'specifies number of rating years'                           
*                                  DT=DAY(-DAY2)/TIME(-TIME2)                   
         DC    C'DAYTIME ',C'DT ',AL1(OPTARTN)                                  
         DC    AL1(3,4,20)                                                      
         DC    AL1(OPTUPDTN),AL4(OPTUPDTB)                                      
         DC    AL2(VALUPDT-DEM00,OPTUPDT-DEMWRKD),AL1(L'OPTUPDT)                
         DC    AL4(OPTUPGDB)                                                    
         DC    AL4(OPTSVISB)                                                    
         DC    CL40'derives demos from given day && time'                       
*                                  BK=MONTH/YEAR                                
         DC    C'BOOK    ',C'BK ',AL1(OPTARTN)                                  
         DC    AL1(2,3,9)                                                       
         DC    AL1(OPTUPBKN),AL4(OPTUPBKB)                                      
         DC    AL2(VALUPBK-DEM00,OPTUPBK-DEMWRKD),AL1(L'OPTUPBK)                
         DC    AL4(OPTUPGDB)                                                    
         DC    AL4(OPTSVISB)                                                    
         DC    CL40'derives shares from given book'                             
*                                  SVI=MONTH                                    
* use optsvi as for WTP instead since optsvi no longer exists                   
* this way we dont have to relink everything                                    
*                                  SVI=MONTH                                    
****     DC    C'SVI     ',C'   ',AL1(OPTATAB+OPTHELPH)                         
         DC    C'WTP     ',C'   ',AL1(OPTATAB+OPTHELPH)                         
         DC    AL1(2,1,3)                                                       
         DC    AL1(OPTSVISN),AL4(OPTSVISB)                                      
         DC    AL2(TBLWTP-DEM00,OPTSVIS-DEMWRKD),AL1(L'OPTSVIS)                 
         DC    AL4(0)                                                           
         DC    AL4(OPTUPGDB)                                                    
****     DC    CL40'indexes demos by SVI month factor'                          
         DC    CL40'Merge weekly homes and sweep demos'                         
*                                  PGM=YES/NO                                   
         DC    C'PROGNAME',C'PGM',AL1(OPTATAB)                                  
         DC    AL1(2,1,3)                                                       
         DC    AL1(OPTPROGN),AL4(OPTPROGB)                                      
         DC    AL2(TBLPROG-DEM00,OPTPROG-DEMWRKD),AL1(L'OPTPROG)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'controls display of program names'                          
*                                  TIME=STANDARD/MILITARY                       
         DC    C'TIME    ',C'TM ',AL1(OPTATAB)                                  
         DC    AL1(2,1,8)                                                       
         DC    AL1(OPTTIMEN),AL4(OPTTIMEB)                                      
         DC    AL2(TBLTIME-DEM00,OPTTIME-DEMWRKD),AL1(L'OPTTIME)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'controls display format of day/time'                        
*                                  LIST=ALPHA/NUMERIC                           
         DC    C'LIST    ',C'LST',AL1(OPTATAB)                                  
         DC    AL1(2,1,7)                                                       
         DC    AL1(OPTLISTN),AL4(OPTLISTB)                                      
         DC    AL2(TBLLIST-DEM00,OPTLIST-DEMWRKD),AL1(L'OPTLIST)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'lists markets in requested sequence'                        
*                                  NAME=XXXXXXXX                                
         DC    C'NAME    ',C'NM ',AL1(OPTARTN)                                  
         DC    AL1(2,1,8)                                                       
         DC    AL1(OPTSTRTN),AL4(OPTSTRTB)                                      
         DC    AL2(VALSTRT-DEM00,OPTSTRT-DEMWRKD),AL1(L'OPTSTRT)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'lists markets starting with input value'                    
*                                  NAME=XXXXXXXX                                
         DC    C'SYSSTART',C'SS ',AL1(OPTARTN)                                  
         DC    AL1(2,1,8)                                                       
         DC    AL1(OPTSTRTN),AL4(OPTSTRTB)                                      
         DC    AL2(VALSTRT-DEM00,OPTSTRT-DEMWRKD),AL1(L'OPTSTRT)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'lists syscodes starting with input value'                   
*                                  PRINT=INS                                    
         DC    C'PRINT   ',C'PRT',AL1(OPTARTN)                                  
         DC    AL1(3,3,3)                                                       
         DC    AL1(OPTPRNTN),AL4(OPTPRNTB)                                      
         DC    AL2(VALPRNT-DEM00,OPTPRNT-DEMWRKD),AL1(L'OPTPRNT)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'copies report to the print queue'                           
*&&DO                                                                           
*                                  TYPE=FILTER                                  
         DC    C'TYPE    ',C'TYP',AL1(OPTARTN)                                  
         DC    AL1(2,1,7)                                                       
         DC    AL1(OPTPTYPN),AL4(OPTPTYPB)                                      
         DC    AL2(VALTYPE-DEM00,OPTPTYP-DEMWRKD),AL1(L'OPTPTYP)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'filters programs of a given type'                           
*                                  DAYPART=FILTER                               
         DC    C'DAYPART ',C'DPT',AL1(OPTARTN)                                  
         DC    AL1(4,1,7)                                                       
         DC    AL1(OPTDPRTN),AL4(OPTDPRTB)                                      
         DC    AL2(VALDPRT-DEM00,OPTDPRT-DEMWRKD),AL1(L'OPTDPRT)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'filters dayparts of a given type'                           
*&&                                                                             
*                                  DATA=SVIS                                    
         DC    C'DATA    ',C'DA ',AL1(OPTATAB)                                  
         DC    AL1(3,1,4)                                                       
         DC    AL1(OPTDTYPN),AL4(OPTDTYPB)                                      
         DC    AL2(TBLDATA-DEM00,OPTDTYP-DEMWRKD),AL1(L'OPTDTYP)                
         DC    AL4(OPTSVISB)                                                    
         DC    AL4(OPTUPGDB)                                                    
         DC    CL40'overrides display data type'                                
*                                  PERIOD=START-END                             
         DC    C'PERIOD  ',C'PER',AL1(OPTARTN)                                  
         DC    AL1(3,2,17)                                                      
         DC    AL1(OPTDATEN),AL4(OPTDATEB)                                      
         DC    AL2(VALDATE-DEM00,OPTDATS-DEMWRKD),AL1(L'OPTDATS)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'defines start && end dates of look-up'                      
*                                  PERIOD=START-END                             
         DC    C'SECONDS ',C'SEC',AL1(OPTARTN)                                  
         DC    AL1(3,1,3)                                                       
         DC    AL1(OPTSECSN),AL4(OPTSECSB)                                      
         DC    AL2(VALSECS-DEM00,OPTSECS-DEMWRKD),AL1(L'OPTSECS)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'filters spots for given seconds length'                     
*                                                                               
*                                  PERIOD=START-END                             
         DC    C'CLIENT  ',C'CLI',AL1(OPTARTN)                                  
         DC    AL1(3,1,3)                                                       
         DC    AL1(OPTCLIN),AL4(OPTCLIB)                                        
         DC    AL2(VALCLI-DEM00,OPTCLI-DEMWRKD),AL1(L'OPTCLI)                   
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'client for demo lookup controls'                            
*                                  FILTER ON SPORTS                             
         DC    C'SPORTS  ',C'SPR',AL1(OPTARTN+OPTHELPH+OPTKONLY)                
         DC    AL1(3,3,6)                                                       
         DC    AL1(OPTYSPRN),AL4(OPTYSPRB)                                      
         DC    AL2(VALSPRT-DEM00,OPTSPRT-DEMWRKD),AL1(L'OPTSPRT)                
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB)                                                    
         DC    CL40'filter on sports programs only'                             
*                                  FILTER W/O SPORTS                            
         DC    C'-SPORTS ',C'-SP',AL1(OPTARTN+OPTHELPH+OPTKONLY)                
         DC    AL1(3,3,7)                                                       
         DC    AL1(OPTNSPRN),AL4(OPTNSPRB)                                      
         DC    AL2(VALSPRT-DEM00,OPTSPRT-DEMWRKD),AL1(L'OPTSPRT)                
         DC    AL4(0)                                                           
         DC    AL4(OPTBESTB)                                                    
         DC    CL40'filter all except sports programs'                          
*                                  PRECISION SPECIFICATION                      
         DC    C'DMA     ',C'   ',AL1(OPTATAB)                                  
         DC    AL1(2,1,3)                                                       
         DC    AL1(OPTDMAN),AL4(OPTDMAB)                                        
         DC    AL2(TBLDMA-DEM00,OPTDMA-DEMWRKD),AL1(L'OPTDMA)                   
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'imp/rtg based precision specification'                      
*                                  PNLIST=ALPHA/NUMERIC                         
         DC    C'PNLIST  ',C'PNL',AL1(OPTATAB)                                  
         DC    AL1(3,1,7)                                                       
         DC    AL1(OPTPNLSN),AL4(OPTPNLSB)                                      
         DC    AL2(TBLLIST-DEM00,OPTLIST-DEMWRKD),AL1(L'OPTLIST)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'lists program info in requested sequence'                   
*                                  PMLIST=MARKET/STATION                        
         DC    C'PMLIST  ',C'PML',AL1(OPTATAB)                                  
         DC    AL1(3,1,7)                                                       
         DC    AL1(OPTPMLSN),AL4(OPTPMLSB)                                      
         DC    AL2(TBLPMLST-DEM00,OPTLIST-DEMWRKD),AL1(L'OPTLIST)               
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'lists program info in requested sequence'                   
*                                  BEST PROGRAMS (PAV FILE)                     
         DC    C'BEST    ',C'   ',AL1(OPTARTN+OPTHELPH+OPTKONLY)                
         DC    AL1(3,3,4)                                                       
         DC    AL1(OPTBESTN),AL4(OPTBESTB)                                      
         DC    AL2(VALBEST-DEM00,OPTBEST-DEMWRKD),AL1(L'OPTBEST)                
         DC    AL4(0)                                                           
         DC    AL4(OPTYSPRB+OPTNSPRB)                                           
         DC    CL40'report best programs'                                       
*                                  NOR PROGRAMS (PAV FILE)                      
         DC    C'NOR     ',C'   ',AL1(OPTARTN+OPTHELPH+OPTKONLY)                
         DC    AL1(3,3,3)                                                       
         DC    AL1(OPTNORN),AL4(OPTNORB)                                        
         DC    AL2(VALNOR-DEM00,OPTNOR-DEMWRKD),AL1(L'OPTNOR)                   
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'show nor programs'                                          
*                                  DISPLAY WEEKS VERTICALLY                     
         DC    C'WKVERT  ',C'   ',AL1(OPTARTN+OPTHELPH+OPTKONLY)                
         DC    AL1(3,3,6)                                                       
         DC    AL1(OPTWKVN),AL4(OPTWKVB)                                        
         DC    AL2(VALWKV-DEM00,OPTWKV-DEMWRKD),AL1(L'OPTWKV)                   
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'always display weeks down the screen'                       
*                                  MULTIPLE BOOKS                               
         DC    C'MBOOKS  ',C'MBK',AL1(OPTARTN)                                  
******   DC    AL1(3,3,39)                                                      
         DC    AL1(3,3,80)                                                      
         DC    AL1(OPTMBKN),AL4(OPTMBKB)                                        
         DC    AL2(VALMBK-DEM00,OPTMBK-DEMWRKD),AL1(OPTMBKL)                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'display average of multiple books'                          
*                                  IN MKT SHARE                                 
         DC    C'IMS     ',C'IMS',AL1(OPTARTN)                                  
         DC    AL1(3,1,39)                                                      
         DC    AL1(OPTIMSN),AL4(OPTIMSB)                                        
         DC    AL2(VALIMS-DEM00,OPTIMS-DEMWRKD),AL1(OPTIMSL)                    
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'calculate in market share'                                  
*                                  CABLE PROGRAM LEVEL AVERAGING                
         DC    C'CAVG    ',C'   ',AL1(OPTATAB)                                  
         DC    AL1(3,1,3)                                                       
         DC    AL1(OPTCAVGN),AL4(OPTCAVGB)                                      
         DC    AL2(TBLCAVG-DEM00,OPTCAVG-DEMWRKD),AL1(L'OPTCAVG)                
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'look up program, track or episode average'                  
*                                  VPH=MONTH/YEAR                               
         DC    C'VPH     ',C'   ',AL1(OPTARTN)                                  
         DC    AL1(2,3,6)                                                       
         DC    AL1(OPTVPHN),AL4(OPTVPHB)                                        
         DC    AL2(VALUPBK-DEM00,OPTVPHBK-DEMWRKD),AL1(L'OPTVPHBK)              
         DC    AL4(0)                                                           
         DC    AL4(0)                                                           
         DC    CL40'derives VPH levels given book'                              
                                                                                
         DC    AL1(EOT)                                                         
*&&                                                                             
*&&DO                                                                           
         TITLE 'DEDEM80 - $DEM TABLES (STTABADD && STTABCLR)'                   
STTABADD DS    0CL(1+2)            TABLE OF DSPLS FOR STEREO ADDRESSES          
         DC    C'C',AL2(GOSTEO-DEM00)                                           
         DC    C'C',AL2(STEREO-DEM00)                                           
         DC    C'C',AL2(GETTDR-DEM00)                                           
         DC    C'C',AL2(VALRTNS-DEM00)                                          
         DC    C'C',AL2(STROIKY-DEM00)                                          
         DC    C'T',AL2(STDEMEXP-DEMTWAD)                                       
         DC    C'I',AL2(STIOBUFF-TIADSECT)                                      
         DC    C'I',AL2(STBUFFER-TIADSECT)                                      
         DC    C'I',AL2(STINCHNK-TIADSECT)                                      
         DC    C'I',AL2(STICHNKX-TIADSECT)                                      
         DC    C'I',AL2(STACTIN-TIADSECT)                                       
         DC    C'I',AL2(APSAV2-TIADSECT)                                        
         DC    C'I',AL2(DCONMKT-TIADSECT)                                       
         DC    C'C',AL2(STEREO2-DEM00)                                          
***      DC    C'I',AL2(0)                                                      
         DC    C'I',AL2(0)                                                      
         DC    C'I',AL2(0)                                                      
         DC    C'I',AL2(0)                                                      
         DC    AL1(EOT)                                                         
STTABADQ EQU   ((*-1)-STTABADD)/(L'STTABADD)                                    
                                                                                
         DS    0CL(STTABADQ-STADDRSQ+1)                                         
         DS    0CL(STADDRSQ-STTABADQ+1)                                         
*                                                                               
STTABCLR DS    0XL(1+2+2)          STORAGES TO CLEAR FOR NEW REQUEST            
         DC    C'T',AL2(DSPRCTS-DEMTWAD),AL2(DSPRCTX-DSPRCTS)                   
         DC    C'T',AL2(SVDBKVAL-DEMTWAD),AL2(SVDBKVLQ)                         
         DC    C'T',AL2(STINCTRS-DEMTWAD),AL2(STINCTRX-STINCTRS)                
         DC    C'T',AL2(REQCTLTB-DEMTWAD),AL2(RQCTLTBX-REQCTLTB)                
         DC    C'T',AL2(STPRVOVL-DEMTWAD),AL2(L'STPRVOVL)                       
         DC    C'T',AL2(STDEMEXP-DEMTWAD),AL2(STDMEXPX-STDEMEXP)                
         DC    C'T',AL2(APSAVE-DEMTWAD),AL2(L'APSAVE)                           
         DC    C'I',AL2(STINCHNK-TIADSECT),AL2(STICHNKX+1-STINCHNK)             
         DC    C'I',AL2(STACTIN-TIADSECT),AL2(STACTINL)                         
         DC    C'I',AL2(STIOBUFF-TIADSECT),AL2(STIOBUFX-STIOBUFF)               
         DC    C'I',AL2(STBUFFER-TIADSECT),AL2(STBUFFRX-STBUFFER)               
         DC    C'I',AL2(APSAV2-TIADSECT),AL2(APSAV2L)                           
STTBCLRQ EQU   (*-STTABCLR)/(L'STTABCLR)                                        
*&&                                                                             
*                                                                               
         TITLE 'DEDEM80 - $DEM TABLES (TABLEN && TABLABEL)'                     
TABLEN   DS    0XL(2+2)            TABLE OF LENGTHS OF BIG AREAS                
         DC    AL2(IOAREA2-IOAREA1),AL2(LIOAREA1-DEMWRKD)                       
         DC    AL2(IOAREA2-IOAREA1),AL2(LIOAREA2-DEMWRKD)                       
         DC    AL2(EBRECX-EBREC),AL2(LEBREC-DEMWRKD)                            
         DC    AL2(APWORKX-APWORK),AL2(LAPWORK-DEMWRKD)                         
         DC    AL2(STIOBUFX-STIOBUFF),AL2(LSTIOBUF-DEMWRKD)                     
         DC    AL2(STBUFFRX-STBUFFER),AL2(LSTBUFFR-DEMWRKD)                     
         DC    AL2(STICHNKX+1-STINCHNK),AL2(LSTINCHK-DEMWRKD)                   
         DC    AL2(STACTINL),AL2(LSTACTIN-DEMWRKD)                              
         DC    AL2(APSAV2L),AL2(LAPSAV2-DEMWRKD)                                
         DC    AL2(TSIORECX-TSIOREC),AL2(LTSIOREC-DEMWRKD)                      
         DC    AL2(DCONMKTL),AL2(LDCONMKT-DEMWRKD)                              
TABLENQ  EQU   (*-TABLEN)/(L'TABLEN)                                            
*                                                                               
TABLABEL DS    0XL(2+8)            TABLE OF LABELS FOR BUFFERS                  
         DC     AL2(ASTINCHK-DEMWRKD),CL8'*STICHK*'                             
         DC     AL2(ASTACTIN-DEMWRKD),CL8'*STACTIN'                             
         DC     AL2(ASTIOBUF-DEMWRKD),CL8'*STIOBF*'                             
         DC     AL2(ASTBUFFR-DEMWRKD),CL8'*STBUFF*'                             
         DC     AL2(AAPSAV2-DEMWRKD),CL8'*APSAV2*'                              
         DC     AL2(ADCONMKT-DEMWRKD),CL8'*DCONMK*'                             
TABLABLQ EQU   ((*-TABLABEL)/L'TABLABEL)                                        
         TITLE 'DEDEM80 - $DEM TABLES (STROIKY)'                                
*&&DO                                                                           
STROIKY  DS    0C                  STEREO INPUT KEYWORDS (SEE STROIKYD)         
                                                                                
         DS    0C                  Action                                       
         DC    AL1(IKNACT),AL1(STKF1SCN)                                        
         DC    AL2(SP@1BACT-DEMTWAD),AL1(L'SP@1BACT)                            
         DC    AL2(ADEMACT-DEMWRKD),AL1(L'DEMACT),AL1(L'DUMACT)                 
         DC    AL2(DSPRCTAC-DEMTWAD)                                            
                                                                                
         DS    0C                  File                                         
         DC    AL1(IKNFIL),AL1(STKF1SCN)                                        
         DC    AL2(SP@1BFIL-DEMTWAD),AL1(L'SP@1BFIL)                            
         DC    AL2(ADEMFIL-DEMWRKD),AL1(L'DEMFIL),AL1(L'DUMFIL)                 
         DC    AL2(DSPRCTFL-DEMTWAD)                                            
                                                                                
         DS    0C                  Source                                       
         DC    AL1(IKNSOU),AL1(STKF1SCN)                                        
         DC    AL2(SP@1BSRC-DEMTWAD),AL1(L'SP@1BSRC)                            
         DC    AL2(ADEMSRC-DEMWRKD),AL1(L'DEMSRC),AL1(L'DUMSRC)                 
         DC    AL2(DSPRCTSC-DEMTWAD)                                            
                                                                                
         DS    0C                  Stations                                     
         DC    AL1(IKNSTA),AL1(STKF1SCN)                                        
         DC    AL2(SP@1BSTA-DEMTWAD),AL1(L'SP@1BSTA)                            
         DC    AL2(ADEMSTN-DEMWRKD),AL1(L'DEMSTN),AL1(L'DUMSTN)                 
         DC    AL2(DSPRCTST-DEMTWAD)                                            
                                                                                
         DS    0C                  Book                                         
         DC    AL1(IKNBOO),AL1(STKF1SCN)                                        
         DC    AL2(SP@1BBOK-DEMTWAD),AL1(L'SP@1BBOK)                            
         DC    AL2(ADEMBOK-DEMWRKD),AL1(L'DEMBOK),AL1(L'DUMBOK)                 
         DC    AL2(DSPRCTBK-DEMTWAD)                                            
                                                                                
         DS    0C                  Day/time                                     
         DC    AL1(IKNDAY),AL1(STKF1SCN+STKF1SBF)                               
*&&DO                                                                           
         DC    AL1(IKNDAY),AL1(STKF1SCN)                                        
*&&                                                                             
         DC    AL2(SP@1BDYT-DEMTWAD),AL1(L'SP@1BDYT)                            
         DC    AL2(ADEMDAT-DEMWRKD),AL1(L'DEMDAT),AL1(L'DUMDAT)                 
         DC    AL2(DSPRCTDT-DEMTWAD)                                            
                                                                                
         DS    0C                  Demos                                        
         DC    AL1(IKNDEM),AL1(STKF1SCN+STKF1SBF)                               
         DC    AL2(SP@1BDEM-DEMTWAD),AL1(L'SP@1BDEM)                            
         DC    AL2(ADEMDEM-DEMWRKD),AL1(L'DEMDEM),AL1(L'DUMDEM)                 
         DC    AL2(DSPRCTDM-DEMTWAD)                                            
                                                                                
         DS    0C                  Options                                      
         DC    AL1(IKNOPT),AL1(STKF1SCN+STKF1SBF)                               
         DC    AL2(SP@1BOPT-DEMTWAD),AL1(L'SP@1BOPT)                            
         DC    AL2(ADEMOPT-DEMWRKD),AL1(L'DEMOPT),AL1(L'DUMOPT)                 
         DC    AL2(DSPRCTOP-DEMTWAD)                                            
                                                                                
         DS    0CL(MAXKYWDS-((*-STROIKY)/STROIKYQ)+1)                           
         DC    AL1(EOT)                                                         
*&&                                                                             
         TITLE 'DEDEM80 - $DEM TABLES (UPCASETB)'                               
UPCASETB DS    0X                  TABLE TO TRANSLATE TO UPPER CASE             
         DC    XL16'000102030405060708090A0B0C0D0E0F'   00 - 0F                 
         DC    XL16'101112131415161718191A1B1C1D1E1F'   10 - 1F                 
         DC    XL16'202122232425262728292A2B2C2D2E2F'   20 - 2F                 
         DC    XL16'303132333435363738393A3B3C3D3E3F'   30 - 3F                 
         DC    XL16'404142434445464748494A4B4C4D4E4F'   40 - 4F                 
         DC    XL16'505152535455565758595A5B5C5D5E5F'   50 - 5F                 
         DC    XL16'606162636465666768696A6B6C6D6E6F'   60 - 6F                 
         DC    XL16'707172737475767778797A7B7C7D7E7F'   70 - 7F                 
         DC    XL16'80C1C2C3C4C5C6C7C8C98A8B8C8D8E8F'   80 - 8F                 
         DC    XL16'90D1D2D3D4D5D6D7D8D99A9B9C9D9E9F'   90 - 9F                 
         DC    XL16'A0A1E2E3E4E5E6E7E8E9AAABACADAEAF'   A0 - AF                 
         DC    XL16'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'   B0 - BF                 
         DC    XL16'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'   C0 - CF                 
         DC    XL16'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'   D0 - DF                 
         DC    XL16'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'   E0 - EF                 
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'   F0 - FF                 
UPCSETBX DS    0X                                                               
         DS    0CL(X'0100'-(UPCSETBX-UPCASETB)+1)                               
         DS    0CL((UPCSETBX-UPCASETB)-X'0100'+1)                               
       ++INCLUDE DEDEMWRK                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066DEDEM81   08/27/14'                                      
         END                                                                    
