*          DATA SET DEDEM01    AT LEVEL 021 AS OF 06/16/08                      
*PHASE T21B01A,*                                                                
         TITLE 'DEDEM01 - $DEM HELP FACILITIES'                                 
************************UPDATE LOG********************************              
*   DATE   LVL PGMR   DESCRIPTION                                     *         
* -------- --- ----   ----------------------------------------------- *         
* NOV02/00 018 BPOO - MOVE DEM TABLES TO DEM81 PHASE                  *         
* SEP22/00 017 BPOO - falink stuff...change screen one line lower     *         
* Mar08/00 016 GLEE - Relinked for bigger buy record I/O area         *         
*                                                                     *         
* Oct29/97 012 GLEE - Remove help stuff for ESTIMATE action           *         
*                                                                     *         
* Oct29/97 011 GLEE - Enhanced help for demos                         *         
*                                                                     *         
* Aug02/96 010 GLEE - Help for multi books                            *         
*                                                                     *         
* Nov15/95 009 GLEE - Preparatory stages for STEREO support           *         
*                                                                     *         
* Jul31/95 008 GLEE - DSECT changed for PAV support                   *         
*                                                                     *         
* Dec05/94 007 GLEE - Added help for PROGMKT action                   *         
*                                                                     *         
* 18JUN87  ADDED HELP FOR NEW ESTIMATE ACTION                    *              
******************************************************************              
DEM01    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEM1**,RA,RR=RE                                              
         USING DEMWRKD,R9          R9=A(GLOBAL W/S)                             
         USING DEMTWAD,R8          R8=A(TWA)                                    
         L     R7,AAPWORK                                                       
         USING DEMTMPD,R7          R7=A(TEMP W/S)                               
         ST    RE,MYRELO                                                        
         L     R6,BINATAB          R6=A(BINSRCH TABLE)                          
         SR    R5,R5               R5=N'BINTAB RECORDS                          
         LA    R0,L'DEMLN1         SET BINTAB RECORD LENGTH                     
         ST    R0,BINLREC                                                       
         L     R1,AACTNTRY         EXTRACT INPUT ACTION                         
         MVC   ACTION,ACTACTN-ACTTABDD(R1)                                      
*                                  HANDLE CONTROLLER MODE SETTINGS              
         CLI   APMODE,FORMHEAD                                                  
         BE    HELPER                                                           
         DC    H'0'                                                             
*                                                                               
HELPEXIT MVI   APMODE,FORMLINE     TELL CONTROLLER I'M DONE                     
         ST    R5,BINSOFAR         AND NUMBER OF RECORDS                        
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
* FIND HELP TYPE & GO TO SPECIFIC HELP ROUTINE                                  
*                                                                               
HELPER   LA    RF,HELPTAB          FIND HELP TYPE IN TABLE                      
HELPER2  CLI   0(RF),EOT           TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                THAT WON'T DO                                
         CLC   HELPREQD,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,L'HELPTAB(RF)                                                 
         B     HELPER2                                                          
         ICM   RF,7,1(RF)                                                       
         A     RF,MYRELO                                                        
         BR    RF                  GO TO SPECIFIC HELP ROUTINE                  
         SPACE 1                                                                
HELPTAB  DS    0F                                                               
         DC    AL1(HELPSYS),AL3(SYSHELP)                                        
         DC    AL1(HELPACT),AL3(ACTHELP)                                        
         DC    AL1(HELPFIL),AL3(FILHELP)                                        
         DC    AL1(HELPSRC),AL3(SRCHELP)                                        
         DC    AL1(HELPSTA),AL3(STAHELP)                                        
         DC    AL1(HELPBK),AL3(BKHELP)                                          
         DC    AL1(HELPDAY),AL3(DAYHELP)                                        
         DC    AL1(HELPDEM),AL3(DEMHELP)                                        
         DC    AL1(HELPOPT),AL3(OPTHELP)                                        
         DC    AL1(EOT),AL3(0)                                                  
         EJECT                                                                  
* HELP FOR SYSTEM                                                               
*                                                                               
SYSHELP  B     EXIT                                                             
         SPACE 1                                                                
* HELP FOR ACTIONS                                                              
*                                                                               
ACTHELP  MVC   DEMHD1(L'ACTHEAD1),ACTHEAD1                                      
         MVC   DEMHD2(L'ACTHEAD2),ACTHEAD2                                      
         L     R2,AACTTAB                                                       
         USING ACTTABDD,R2         R2=A(ACTION TABLE)                           
ACTHELP2 CLI   ACTNAME,EOT         TEST E-O-T                                   
         BE    ACTHELPX                                                         
         TM    ACTINDS,DDSONLY     TEST DDS ONLY ACTION                         
         BZ    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   ACTHELP6                                                         
         MVC   0(L'DEMLN1,R6),SPACES                                            
         MVC   0(L'ACTNAME,R6),ACTNAME                                          
         MVC   10(L'ACTINFO,R6),ACTINFO                                         
         TM    ACTI,DEFAULT        TEST IF THIS IS THE DEAULT ACTION            
         BZ    ACTHELP4                                                         
         CLC   ACTNAME,ACTD                                                     
         BNE   ACTHELP4                                                         
         LA    R1,L'ACTINFO+9(R6)  YES - TACK ON DEFAULT KEYWORD                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'DEFWORD,R1),DEFWORD                                          
ACTHELP4 LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
ACTHELP6 LA    R2,ACTTABLL(R2)                                                  
         B     ACTHELP2                                                         
ACTHELPX B     HELPEXIT                                                         
         DROP  R2                                                               
         EJECT                                                                  
* HELP FOR FILES                                                                
*                                                                               
FILHELP  MVC   DEMHD1(L'FILHEAD1),FILHEAD1                                      
         MVC   DEMHD2(L'FILHEAD2),FILHEAD2                                      
         L     R2,AFILTAB                                                       
         USING FILTABD,R2          R2=A(FILE TABLE)                             
         MVI   DUB,0                                                            
FILHELP2 CLI   FILNAME,EOT         TEST E-O-T                                   
         BE    FILHELPX                                                         
         CLI   FILACTN,0           TEST FILE IS ACTION SPECIFIC                 
         BE    FILHELP3                                                         
         CLC   FILACTN,ACTION                                                   
         BNE   FILHELP6                                                         
         OI    DUB,1                                                            
FILHELP3 TM    DUB,1                                                            
         BZ    *+14                                                             
         CLC   FILACTN,ACTION                                                   
         BNE   FILHELP6                                                         
         TM    FILINDS,DDSONLY     TEST DDS ONLY FILE                           
         BZ    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   FILHELP6                                                         
         TM    FILI,PRESET         TEST FILE PRESET FOR ACTION                  
         BZ    *+14                                                             
         CLC   FILNAME,FILD                                                     
         BNE   FILHELP6                                                         
         MVC   0(L'DEMLN1,R6),SPACES                                            
         MVC   1(L'FILNAME,R6),FILNAME                                          
         MVC   7(L'FILINFO,R6),FILINFO                                          
         TM    FILI,DEFAULT        TEST IF THIS IS THE DEFAULT FILE             
         BZ    FILHELP4                                                         
         CLC   FILNAME,FILD                                                     
         BNE   FILHELP4                                                         
         LA    R1,L'FILINFO+6(R6)  YES - TACK ON DEFAULT KEYWORD                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'DEFWORD,R1),DEFWORD                                          
FILHELP4 LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
FILHELP6 LA    R2,FILTABL(R2)                                                   
         B     FILHELP2                                                         
FILHELPX B     HELPEXIT                                                         
         DROP  R2                                                               
         EJECT                                                                  
* HELP FOR SOURCES                                                              
*                                                                               
SRCHELP  MVC   DEMHD1(L'SRCHEAD1),SRCHEAD1                                      
         MVC   DEMHD2(L'SRCHEAD2),SRCHEAD2                                      
         L     R2,AFMSTAB                                                       
         USING FMSTABD,R2          R2=A(FILE/SOURCE TABLE)                      
         MVI   DUB,0                                                            
SRCHELP2 CLI   FMSFIL,EOT          TEST E-O-T                                   
         BE    SRCHELPX                                                         
         CLI   FMSACTN,0           TEST SOURCE IS ACTION SPECIFIC               
         BZ    SRCHELP3                                                         
         CLC   FMSACTN,ACTION                                                   
         BNE   SRCHELP6                                                         
         OI    DUB,1                                                            
SRCHELP3 TM    DUB,1                                                            
         BZ    *+14                                                             
         CLC   FMSACTN,ACTION                                                   
         BNE   SRCHELP6                                                         
         CLC   FMSFIL,FILE         MATCH ON THIS FILE ONLY                      
         BNE   SRCHELP6                                                         
         CLI   FMSACTN,0           TEST SOURCE IS ACTION SPECIFIC               
         BE    *+14                                                             
         CLC   FMSACTN,ACTION                                                   
         BNE   SRCHELP6                                                         
         TM    FMSINDS,DDSONLY     TEST DDS ONLY FILE/SOURCE                    
         BZ    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   SRCHELP6                                                         
         TM    SRCI,DEFAULT+PRESET                                              
         BO    SRCHELP4                                                         
         TM    SRCI,PRESET         TEST PRESET SOURCE                           
         BZ    *+14                                                             
         CLC   FMSSRC,SRCD                                                      
         BNE   SRCHELP6                                                         
SRCHELP4 MVC   0(L'DEMLN1,R6),SPACES                                            
         MVC   2(L'FMSSRC,R6),FMSSRC                                            
         MVC   9(L'FMSINFO,R6),FMSINFO                                          
         TM    SRCI,DEFAULT+PRESET                                              
         BO    SRCHELP5                                                         
         TM    SRCI,DEFAULT        TEST IF THIS IS THE DEFAULT SOURCE           
         BZ    SRCHELP5                                                         
         CLC   FMSSRC,SRCD                                                      
         BNE   SRCHELP5                                                         
         LA    R1,L'FMSINFO+8(R6)  YES - TACK ON DEFAULT KEYWORD                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'DEFWORD,R1),DEFWORD                                          
SRCHELP5 LA    R6,L'DEMHD1(R6)                                                  
         LA    R5,1(R5)                                                         
SRCHELP6 LA    R2,FMSTABL(R2)                                                   
         B     SRCHELP2                                                         
SRCHELPX B     HELPEXIT                                                         
         DROP  R2                                                               
         EJECT                                                                  
* HELP FOR STATIONS                                                             
*                                                                               
STAHELP  MVC   DEMHD1(L'STAHEAD1),STAHEAD1                                      
         MVC   DEMHD2(L'STAHEAD2),STAHEAD2                                      
         CLI   STAN,0              NO STATIONS VALID                            
         BNE   STAHELP2                                                         
         MVC   0(L'DEMLN1,R6),STANONE                                           
         LA    R5,1(R5)                                                         
         B     STAHELPX                                                         
STAHELP2 CLI   STAN,1              ONE OR MORE STATIONS VALID                   
         BNE   *+14                                                             
         MVC   0(L'DEMLN1,R6),STAONE                                            
         B     STAHELP4                                                         
*&&DO                                                                           
         TM    STAV,STAVESTS       ESTIMATE ACTION                              
         BZ    *+12                                                             
         CLI   PROGPROF,C'N'       AND NEW FORMAT                               
         BE    STAHELP4            JUST ONE STATION                             
*&&                                                                             
         MVC   0(L'DEMLN1,R6),STAMORE                                           
         MVC   6(1,R6),STAN                                                     
         OI    6(R6),X'F0'                                                      
STAHELP4 LA    R1,STAINFO                                                       
*                                                                               
*&&DO                                                                           
         TM    STAV,STAVESTS       ESTIMATE ACTION                              
         BZ    STAHELP5                                                         
         CLI   PROGPROF,C'N'       NEW FORMAT REQUESTED                         
         BNE   *+8                                                              
         LA    R1,STAINFO2         YES - POINT TO IT                            
*                                                                               
*&&                                                                             
STAHELP5 IC    RE,STAV                                                          
         LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
STAHELP6 CLI   0(R1),X'FF'                                                      
         BE    STAHLP10                                                         
         CLI   0(R1),0             APPLIES TO ALL ACTIONS                       
         BE    STAHELP8                                                         
         EX    RE,*+8              TEST VALID FOR THIS ACTION                   
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    STAHELPA                                                         
STAHELP8 MVC   0(L'DEMHD1,R6),1(R1)                                             
         LA    R6,L'DEMHD1(R6)                                                  
         LA    R5,1(R5)                                                         
STAHELPA LA    R1,L'STAINFO(R1)                                                 
         B     STAHELP6                                                         
*                                                                               
STAHLP10 DS    0H                                                               
         IC    RE,STV2                                                          
         LA    R1,STAINFO3                                                      
         L     RF,AACTNTRY                                                      
STAHLP12 CLI   0(R1),X'FF'                                                      
         BE    STAHLP20                                                         
         EX    RE,*+8              TEST VALID FOR THIS STTN VALIDATOR           
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    STAHLP16                                                         
         CLC   1(1,R1),ACTACTN-ACTTABDD(RF)  MATCH ON ACTN CODE AS WELL         
         BNE   STAHLP16                                                         
STAHLP14 MVC   0(L'DEMHD1,R6),1(R1)                                             
         LA    R6,L'DEMHD1(R6)                                                  
         LA    R5,1(R5)                                                         
STAHLP16 LA    R1,L'STAINFO3(R1)                                                
         B     STAHLP12                                                         
*                                                                               
STAHLP20 DS    0H                                                               
*                                                                               
STAHELPX B     HELPEXIT                                                         
         EJECT                                                                  
* HELP FOR BOOKS                                                                
*                                                                               
BKHELP   MVC   DEMHD1(L'BKHEAD1),BKHEAD1                                        
         MVC   DEMHD2(L'BKHEAD2),BKHEAD2                                        
         CLI   BKN,0               NO BOOKS VALID                               
         BNE   BKHELP2                                                          
         MVC   0(L'DEMLN1,R6),BKNONE                                            
         LA    R5,1(R5)                                                         
         B     BKHELPX                                                          
BKHELP2  CLI   BKN,1               ONLY ONE BOOK ALLOWED                        
         BNE   *+14                                                             
         MVC   0(L'DEMLN1,R6),BKONE                                             
         B     BKHELP4                                                          
         MVC   0(L'DEMLN1,R6),BKMORE                                            
         MVC   6(1,R6),BKN                                                      
         OI    6(R6),X'F0'                                                      
BKHELP4  LA    R1,BKINFO                                                        
         LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
         MVC   DUB+4(4),=AL4(OPTUPGDB)   TEST WHETHER UPGRADES ALLOWED          
         MVC   DUB(4),OPTX                                                      
         NC    DUB(4),DUB+4                                                     
         BNZ   *+8                                                              
         OI    BKV,BKVESTIM        YES - SET SPECIAL INDICATOR                  
         IC    RE,BKV                                                           
BKHELP6  CLI   0(R1),X'FF'                                                      
         BE    BKHLP050                                                         
         CLI   0(R1),0             APPLIES TO ALL ACTIONS                       
         BE    BKHELP8                                                          
         EX    RE,*+8              TEST VALID FOR THIS ACTION                   
         B     *+8                                                              
         TM    0(R1),0                                                          
         BZ    BKHELPA                                                          
BKHELP8  MVC   0(L'DEMHD1,R6),1(R1)                                             
         LA    R6,L'DEMHD1(R6)                                                  
         LA    R5,1(R5)                                                         
BKHELPA  LA    R1,L'BKINFO(R1)                                                  
         B     BKHELP6                                                          
         EJECT                                                                  
*                                                                               
** DISPLAY SOME MORE BOOK INFO **                                               
*                                                                               
BKHLP050 DS    0H                                                               
         LA    R2,BKINFO2          POINT TO ENTRIES OF BOOK INFO                
                                                                                
BKHLP052 DS    0H                  R2-->HEADER INFO OF ENTRY                    
         ZICM  R0,0(R2),(3)        R0 = LENGTH OF ENTRY                         
         BZ    BKHELPX              EXIT IF NO MORE ENTRIES                     
                                                                                
*                                                                               
         LA    RE,4(R2)            RE-->CRITERIA SECTION                        
         USING BKCRITD,RE                                                       
                                                                                
         ZICM  RF,2(R2),(3)                                                     
         AR    RF,R2               RF-->ACTUAL BOOK INFO FOR DISPLAY            
*                                                                               
BKHLP062 DS    0H                  SEE IF USER REQUST FULFILLS CRITERIA         
         CLI   BKCACT,0                                                         
         BE    *+18                                                             
         L     R1,AACTNTRY                                                      
         CLC   BKCACT,ACTACTN-ACTTABDD(R1) MATCH ON ACTION                      
         BNE   BKHLP067                                                         
                                                                                
         OC    BKCFIL,BKCFIL                                                    
         BZ    *+14                                                             
         CLC   BKCFIL,DBFIL                MATCH ON FILE                        
         BNE   BKHLP067                                                         
                                                                                
         CLI   BKCSRC,0                                                         
         BE    *+14                                                             
         CLC   BKCSRC,DBSRC                MATCH ON SOURCE                      
         BNE   BKHLP067                                                         
                                                                                
         CLI   BKCMED,0                                                         
         BE    *+14                                                             
         CLC   BKCMED,DBMED                MATCH ON MEDIA                       
         BNE   BKHLP067                                                         
                                                                                
         B     BKHLP070            PASSED CRITERIA--DISPLAY THIS ENTRY          
*                                                                               
BKHLP067 DS    0H                  BUMP TO NEXT CRITERIA                        
         LA    RE,BKCRITL(RE)                                                   
         CR    RE,RF               RE POINTS TO A CRITERIA ENTRY?               
         BL    BKHLP062             YEP!                                        
         B     BKHLP080             NOPE, BUMP TO NEXT BOOK INFO ENTRY          
         DROP  RE                                                               
                                                                                
*                                                                               
BKHLP070 DS    0H                  PUT BOOK INFO IN BUFFER                      
         LR    RE,RF                RE-->START OF ACTUAL INFO                   
         LR    RF,R0                                                            
         AR    RF,R2                RF-->END OF ACTUAL INFO                     
*                                                                               
BKHLP072 DS    0H                                                               
         CR    RE,RF               AT END OF ACTUAL INFO?                       
         BNL   BKHLP080             YEP, BUMP TO NEXT BOOK INFO ENTRY           
                                                                                
         MVC   0(L'DEMLN1,R6),0(RE)                                             
         LA    RE,L'DEMLN1(RE)                                                  
         LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
         B     BKHLP072                                                         
                                                                                
*                                                                               
BKHLP080 DS    0H                  BUMP TO NEXT BOOK INFO ENTRY                 
         AR    R2,R0                R0 = LENGTH OF CURRENT ENTRY                
         B     BKHLP052                                                         
                                                                                
*                                                                               
BKHELPX  B     HELPEXIT                                                         
         EJECT                                                                  
* HELP FOR DAY/TIMES                                                            
*                                                                               
DAYHELP  MVC   DEMHD1(L'DAYHEAD1),DAYHEAD1                                      
         MVC   DEMHD2(L'DAYHEAD2),DAYHEAD2                                      
         CLI   DAYN,0              NO DAY/TIMES VALID                           
         BNE   DAYHELP2                                                         
         MVC   0(L'DEMLN1,R6),DAYNONE                                           
         LA    R5,1(R5)                                                         
         B     DAYHELPX                                                         
DAYHELP2 CLI   DAYN,1              ONE OR MORE DAY/TIMES VALID                  
         BNE   *+14                                                             
         MVC   0(L'DEMLN1,R6),DAYONE                                            
         B     DAYHELP4                                                         
         MVC   0(L'DEMLN1,R6),DAYMORE                                           
         ZIC   R0,DAYN                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R6),DUB                                                      
         CLI   6(R2),C'0'                                                       
         BNE   *+10                                                             
         MVC   6(72,R6),7(R6)                                                   
DAYHELP4 LA    R1,DAYINFO                                                       
         LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
DAYHELP6 CLI   0(R1),X'FF'                                                      
         BE    DAYHELPX                                                         
         MVC   0(L'DEMLN1,R6),0(R1)                                             
         LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
         LA    R1,L'DAYINFO(R1)                                                 
         B     DAYHELP6                                                         
DAYHELPX B     HELPEXIT                                                         
         EJECT                                                                  
* HELP FOR DEMOS                                                                
*                                                                               
DEMHELP  MVC   DEMHD1(L'DEMHEAD1),DEMHEAD1                                      
         MVC   DEMHD2(L'DEMHEAD2),DEMHEAD2                                      
         CLI   DEMN,0              NO DEMOS VALID                               
         BNE   DEMHELP2                                                         
         MVC   0(L'DEMLN1,R6),DEMNONE                                           
         LA    R5,1(R5)                                                         
         B     DEMHELPX                                                         
DEMHELP2 ZIC   R1,DEMN             CALCULATE MAX N'DEMOS                        
         SR    R0,R0                                                            
         ZIC   RE,NSTAS                                                         
         DR    R0,RE               MAX DEMOS/N'STAS=N'DEMOS                     
         STC   R1,DEMN                                                          
         CLI   DEMN,1              ONE OR MORE DEMOS VALID                      
         BNE   *+14                                                             
         MVC   0(L'DEMLN1,R6),DEMONE                                            
         B     DEMHELP4                                                         
         MVC   0(L'DEMLN1,R6),DEMMORE                                           
         MVC   6(1,R6),DEMN                                                     
         OI    6(R6),X'F0'                                                      
DEMHELP4 LA    R1,DEMINFO                                                       
         LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
DEMHELP6 CLI   0(R1),X'FF'                                                      
         BE    DMHLP050                                                         
         MVC   0(L'DEMHD1,R6),0(R1)                                             
         LA    R6,L'DEMHD1(R6)                                                  
         LA    R5,1(R5)                                                         
         LA    R1,L'DEMINFO(R1)                                                 
         B     DEMHELP6                                                         
         EJECT                                                                  
*                                                                               
** DISPLAY SOME MORE DEMO INFO **                                               
*                                                                               
DMHLP050 DS    0H                                                               
         LA    R2,DMINFO2          POINT TO ENTRIES OF DEMO INFO                
                                                                                
DMHLP052 DS    0H                  R2-->HEADER INFO OF ENTRY                    
         ZICM  R0,0(R2),(3)        R0 = LENGTH OF ENTRY                         
         BZ    DEMHELPX                                                         
                                                                                
*                                                                               
         LA    RE,4(R2)            RE-->CRITERIA SECTION                        
         USING DMCRITD,RE                                                       
                                                                                
         ZICM  RF,2(R2),(3)                                                     
         AR    RF,R2               RF-->ACTUAL DEMO INFO FOR DISPLAY            
*                                                                               
DMHLP062 DS    0H                  SEE IF USER REQUST FULFILLS CRITERIA         
         CLI   DMCSYS,0                                                         
         BE    *+18                                                             
         L     R1,ASYSNTRY                                                      
         CLC   DMCSYS,SYSINDS-SYSTABD(R1)  MATCH ON SYSTEM                      
         BNE   DMHLP067                                                         
                                                                                
         CLI   DMCACT,0                                                         
         BE    *+18                                                             
         L     R1,AACTNTRY                                                      
         CLC   DMCACT,ACTACTN-ACTTABDD(R1) MATCH ON ACTION                      
         BNE   DMHLP067                                                         
                                                                                
         OC    DMCFIL,DMCFIL                                                    
         BZ    *+14                                                             
         CLC   DMCFIL,DBFIL                MATCH ON FILE                        
         BNE   DMHLP067                                                         
                                                                                
         CLI   DMCSRC,0                                                         
         BE    *+14                                                             
         CLC   DMCSRC,DBSRC                MATCH ON SOURCE                      
         BNE   DMHLP067                                                         
                                                                                
         CLI   DMCMED,0                                                         
         BE    *+14                                                             
         CLC   DMCMED,DBMED                MATCH ON MEDIA                       
         BNE   DMHLP067                                                         
                                                                                
         B     DMHLP070            PASSED CRITERIA--DISPLAY THIS ENTRY          
*                                                                               
DMHLP067 DS    0H                  BUMP TO NEXT CRITERIA                        
         LA    RE,DMCRITL(RE)                                                   
         CR    RE,RF               RE POINTS TO A CRITERIA ENTRY?               
         BL    DMHLP062             YEP!                                        
         B     DMHLP080             NOPE, BUMP TO NEXT DEMO INFO ENTRY          
         DROP  RE                                                               
                                                                                
*                                                                               
DMHLP070 DS    0H                  PUT DEMO INFO INTO BUFFER                    
         LR    RE,RF                RE-->START OF ACTUAL INFO                   
         LR    RF,R0                                                            
         AR    RF,R2                RF-->END OF ACTUAL INFO                     
*                                                                               
DMHLP072 DS    0H                                                               
         CR    RE,RF               AT END OF ACTUAL INFO?                       
         BNL   DMHLP080             YEP, BUMP TO NEXT DEMO INFO ENTRY           
                                                                                
         MVC   0(L'DEMLN1,R6),0(RE)                                             
         LA    RE,L'DEMLN1(RE)                                                  
         LA    R6,L'DEMLN1(R6)                                                  
         LA    R5,1(R5)                                                         
         B     DMHLP072                                                         
                                                                                
*                                                                               
DMHLP080 DS    0H                  BUMP TO NEXT BOOK INFO ENTRY                 
         AR    R2,R0                R0 = LENGTH OF CURRENT ENTRY                
         B     DMHLP052                                                         
                                                                                
*                                                                               
DEMHELPX B     HELPEXIT                                                         
         EJECT                                                                  
* HELP FOR A SPECIFIC OPTION                                                    
*                                                                               
OPTHELP  MVC   DEMHD1(L'OPTHEAD1),OPTHEAD1                                      
         MVC   DEMHD2(L'OPTHEAD2),OPTHEAD2                                      
         L     R2,AOPTTAB                                                       
         USING OPTTABD,R2          R2=A(OPTIONS TABLE)                          
OPTHELP2 CLI   OPTNAME,EOT         TEST E-O-T                                   
         BE    OPTHELPX                                                         
         TM    OPTINDS,DDSONLY     TEST DDS ONLY OPTION                         
         BZ    *+12                                                             
         CLI   DDS,C'Y'                                                         
         BNE   OPTHELPN                                                         
         MVC   DUB(4),OPTX         TEST OPTION IS COMPATIBLE                    
         NC    DUB(4),OPTOPTB                                                   
         BNZ   OPTHELPN                                                         
         SR    R1,R1                                                            
         ICM   R1,3,OPTIADDR                                                    
         A     R1,ABASE                                                         
         ST    R1,IADDR                                                         
         MVC   0(L'DEMLN1,R6),SPACES                                            
         LA    R8,L'DEMLN1(R6)     R8=A(LINE2)                                  
         MVC   0(L'DEMLN1,R8),SPACES                                            
         LA    R1,OPTNAME+L'OPTNAME-1                                           
         CLI   0(R1),C' '          CALCULATE L'OPTNAME                          
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,OPTNAME                                                       
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),OPTNAME     MOVE OUT FULL OPTION NAME                    
         LA    R1,1(R1)            R1=L'LONG NAME                               
         ZIC   RE,OPTMINKL         RE=MINIMUM L'KEYWORD ALLOWED                 
         CR    R1,RE               TEST SHORT VERSION ALLOWED                   
         BL    OPTHELP4                                                         
         LA    RF,0(R6,RE)         YES SHOW OPTIONAL BIT IN PARENS              
         SR    R1,RE                                                            
         LA    RE,OPTNAME(RE)                                                   
         MVI   0(RF),C'('                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(RE)                                                    
         LA    RF,2(RF,R1)                                                      
         MVI   0(RF),C')'                                                       
OPTHELP4 CLC   OPTSHRT,SPACES      TEST SHORT KEWORD IF PRESENT                 
         BE    *+10                                                             
         MVC   1(2,R8),=C'or'                                                   
         MVC   4(L'OPTSHRT,R8),OPTSHRT                                          
         MVC   11(L'OPTINFO,R6),OPTINFO                                         
         MVC   DUB(4),OPTR         TEST IF THIS OPTION IS REQUIRED              
         NC    DUB(4),OPTOPTB                                                   
         BZ    OPTHELP8                                                         
         LA    R1,L'OPTINFO+10(R6) YES - TACK ON REQUIRED KEYWORD               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'REQWORD,R1),REQWORD                                          
OPTHELP8 TM    OPTINDS,OPTHELPH    TEST HELP INFO (AT FRONT OF TABLE)           
         BO    *+12                                                             
         TM    OPTINDS,OPTATAB     TEST IF A ROUTINE                            
         BO    OPTHELPA                                                         
         MVC   11(7,R8),=C'FORMAT='                                             
         L     R1,IADDR                                                         
         MVC   18(60,R8),0(R1)     HELP INFO IS AT A(ROUTINE)                   
         B     OPTHELPG                                                         
OPTHELPA MVC   11(8,R8),=C'VALUES='                                             
         L     R6,IADDR            R6=A(VALUES TABLE)                           
         ZIC   R3,0(R6)            R3=L'LHS OF TABLE ENTRY                      
         ZIC   R0,1(R6)                                                         
         AR    R0,R3                                                            
         ST    R0,DUB              DUB(4)=TOTAL TABLE LENGTH                    
         LA    R4,18(R8)           R4=A(OUTPUT VALUES)                          
         LA    R6,2(R6)            R6=A(FIRST TABLE ENTRY)                      
OPTHELPC CLI   0(R6),EOT           TEST E-O-T                                   
         BNE   *+14                                                             
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          YES - REMOVE TRAILING DELIMITER              
         B     OPTHELPG            GO ON TO NEXT OPTION                         
         LA    R1,0(R6,R3)                                                      
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LR    R0,R6                                                            
         SR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R6)       DISPLAY FULL VALUE                           
         LA    R1,1(R1)                                                         
         LA    RE,0(R4,R1)                                                      
         CLM   R1,1,OPTMINKL       TEST IF FULL VALUE REQUIRED                  
         BL    OPTHELPE                                                         
         ZIC   RE,OPTMINDL         NO - SHOW OPTIONAL BIT IN PARENS             
         LA    RF,0(R4,RE)                                                      
         SR    R1,RE                                                            
         LA    RE,0(R6,RE)                                                      
         MVI   0(RF),C'('                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),0(RE)                                                    
         LA    RF,2(RF,R1)                                                      
         MVI   0(RF),C')'                                                       
         LA    RE,1(RF)                                                         
OPTHELPE MVI   0(RE),C'/'          INSERT DELIMITER                             
         LA    R4,1(RE)                                                         
         A     R6,DUB              BUMP TO NEXT VALUE ENTRY                     
         B     OPTHELPC                                                         
OPTHELPG LA    R6,L'DEMLN1(R8)                                                  
         LA    R5,2(R5)                                                         
OPTHELPN LA    R2,OPTTABL(R2)                                                   
         B     OPTHELP2                                                         
OPTHELPX B     HELPEXIT                                                         
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         SPACE 1                                                                
DEFWORD  DC    C'(DEFAULT)'                                                     
REQWORD  DC    C'(REQUIRED)'                                                    
*                                                                               
ACTHEAD1 DC    C' Valid    Action descriptions'                                 
ACTHEAD2 DC    C'Actions   -------------------'                                 
*                                                                               
FILHEAD1 DC    C'Valid  File descriptions'                                      
FILHEAD2 DC    C'Files  -----------------'                                      
*                                                                               
SRCHEAD1 DC    C' Valid   Source descriptions'                                  
SRCHEAD2 DC    C'Sources  -------------------'                                  
*                                                                               
OPTHEAD1 DC    C' Valid     Option descriptions/formats/values'                 
OPTHEAD2 DC    C'Options    ----------------------------------'                 
         EJECT                                                                  
* STATION INFO                                                                  
*                                                                               
STAHEAD1 DC    C'Valid station formats'                                         
STAHEAD2 DC    C'---------------------'                                         
*                                                                               
STANONE  DC    C'No input in this field is required for '                       
         DC    C'the action you have requested.         '                       
STAONE   DC    C'One station may be input in one of the '                       
         DC    C'following formats -                    '                       
STAMORE  DC    C'Up to   stations may be input (separate'                       
         DC    C'd by commas) in the following formats -'                       
*                                                                               
STAINFO  DS    0CL79                                                            
         DC    AL1(0)                                                           
         DC    C'                                       '                       
         DC    C'                                       '                       
         DC    AL1(0)                                                           
         DC    C'Station type               Input format'                       
         DC    C'                                       '                       
         DC    AL1(0)                                                           
         DC    C'------------               ------------'                       
         DC    C'                                       '                       
*&&DO                                                                           
         DC    AL1(STAVESTS)                                                    
         DC    C'STATION/BUYING PERIOD      Call letters'                       
         DC    C' followed by a ''/'' followed by buying  '                     
         DC    AL1(STAVESTS)                                                    
         DC    C'                           period numbe'                       
         DC    C'r (1 thru 4). (e.g. WABC/1)            '                       
*&&                                                                             
         DC    AL1(STAVSTAS)                                                    
         DC    C'PARENT STATION             3 or 4 chara'                       
         DC    C'cters as applicable. (e.g. WABC)       '                       
         DC    AL1(STAVTOTS)                                                    
         DC    C'MARKET NUMBER              1 thru 4 num'                       
         DC    C'eric market number.  (e.g. 12 123 1234)'                       
         DC    AL1(STAVSPIL)                                                    
         DC    C'SPILL STATION              Call letters'                       
         DC    C' followed by a ''/'' followed by spill   '                     
         DC    AL1(STAVSPIL)                                                    
         DC    C'                           market numbe'                       
         DC    C'r. (e.g. WABC/123)                     '                       
         DC    AL1(STAVPARS)                                                    
         DC    C'SATELLITE STATION          4 character '                       
         DC    C'station call letters (with a space if  '                       
         DC    AL1(STAVPARS)                                                    
         DC    C'                           neccessary) '                       
         DC    C'followed by a ''1'' for Parent+S1 data or'                     
         DC    AL1(STAVPARS)                                                    
         DC    C'                           a ''2'' for Pa'                     
         DC    C'rent+S2 data. (e.g. WABC1 WABC2)       '                       
         DC    AL1(STAVBUYD)                                                    
         DC    C'BUY KEY                    Enter buy ke'                       
         DC    C'y values in the following format -     '                       
         DC    AL1(STAVBUYD)                                                    
         DC    C'                           (Media,)clie'                       
         DC    C'nt,product,estimate,station,line#      '                       
         DC    AL1(STAVBUYD)                                                    
         DC    C'                           e.g. T,ABC,P'                       
         DC    C'OL,1,WABC,20 or ABC,POL,1,WABC,20      '                       
         DC    AL1(255)                                                         
*&&DO                                                                           
*                                                                               
STAINFO2 DS    0CL79                                                            
         DC    AL1(0)                                                           
         DC    C'                                       '                       
         DC    C'                                       '                       
         DC    AL1(0)                                                           
         DC    C'Station type               Input format'                       
         DC    C'                                       '                       
         DC    AL1(0)                                                           
         DC    C'------------               ------------'                       
         DC    C'                                       '                       
         DC    AL1(STAVESTS)                                                    
         DC    C'STATION/BUYING PERIOD      Call letters'                       
         DC    C' followed by a '',''                     '                     
         DC    AL1(STAVESTS)                                                    
         DC    C'                           OPTIONAL (Sc'                       
         DC    C'heme followed by a '','')                '                     
         DC    AL1(STAVESTS)                                                    
         DC    C'                           Buying perio'                       
         DC    C'd (e.g. Q1) optionally followed by a   '                       
         DC    AL1(STAVESTS)                                                    
         DC    C'                           ''/'' and a Ye'                     
         DC    C'ar (e.g. Q1/87)                        '                       
         DC    AL1(255)                                                         
*&&                                                                             
*                                                                               
** Station info for STV2 **                                                     
*                                                                               
STAINFO3 DS    0CL80                                                            
         DC    AL1(STV2NMRC),AL1(PROGMKT)                                       
         DC    C'PROGRAM NUMBER             A program nu'                       
         DC    C'mber in the range 1-16777215           '                       
         DC    XL1'FF'                                                          
         EJECT                                                                  
* BOOK INFO                                                                     
*                                                                               
BKHEAD1  DC    C'Valid book formats'                                            
BKHEAD2  DC    C'------------------'                                            
*                                                                               
BKNONE   DC    C'No input in this field is required for '                       
         DC    C'the action you have requested.         '                       
BKONE    DC    C'One book may be input in one of the fol'                       
         DC    C'lowing formats -                       '                       
BKMORE   DC    C'Up to   books may be input (separated b'                       
         DC    C'y commas) in the following formats -   '                       
*                                                                               
BKVESTIM EQU   X'40'               EQUATE FOR ESTIMATED BOOK                    
BKINFO   DS    0CL79                                                            
         DC    AL1(0)                                                           
         DC    C'                                       '                       
         DC    C'                                       '                       
         DC    AL1(0)                                                           
         DC    C'Book type       Input format           '                       
         DC    C'                                       '                       
         DC    AL1(0)                                                           
         DC    C'---------       ------------           '                       
         DC    C'                                       '                       
         DC    AL1(BKVLATST)                                                    
         DC    C'LATEST          Input the word ''LATEST'''                     
         DC    C' (minimum length one character) to get '                       
         DC    AL1(BKVLATST)                                                    
         DC    C'                the latest book for a s'                       
         DC    C'tation or market. (e.g. L LAT)         '                       
         DC    AL1(BKVESTIM)                                                    
         DC    C'ESTIMATED       Input the word ''ESTIMAT'                      
         DC    C'E'' (minimum length one character) for  '                      
         DC    AL1(BKVESTIM)                                                    
         DC    C'                projecting demos with a'                       
         DC    C' demo upgrade. (e.g. E EST)            '                       
         DC    AL1(BKVMONTH)                                                    
         DC    C'SPECIFIC        Input book month and ye'                       
         DC    C'ar (optionally separated by a ''/'').    '                     
         DC    AL1(BKVMONTH)                                                    
         DC    C'                Months may be shortened'                       
         DC    C' to one character where - M or MA are  '                       
         DC    AL1(BKVMONTH)                                                    
         DC    C'                assumed to be May and J'                       
         DC    C' or Ju are assumed to be July.         '                       
         DC    AL1(BKVMONTH)                                                    
         DC    C'                (e.g. F83=Feb/83, JU/83'                       
         DC    C'=July/83, JUN/83=June/83)              '                       
         DC    AL1(255)                                                         
         EJECT                                                                  
* MORE BOOK INFO                                                                
                                                                                
BKINFO2  DS    0C                                                               
         DC    AL2(BKINFO2X-BKINFO2),AL2(BKINFO2L-BKINFO2)                      
*                                                                               
         DC    AL1(DEMOPER),AL3(0),AL1(0),AL1(0)                                
         DC    AL1(DEMOAVL),AL3(0),AL1(0),AL1(0)                                
*                                                                               
BKINFO2L DS    0C                                                               
         DC    C'MULTIBOOK       Input the letters ''MBK(S)'' to get an+        
                average of up to 4       '                                      
         DC    C'                books.  An entry in the options field +        
               is also required.       '                                        
BKINFO2X EQU   *                                                                
BKINFO2Q EQU   ((BKINFO2X-BKINFO2L)/(L'DEMLN1))*L'DEMLN1                        
         DS    0CL((BKINFO2X-BKINFO2L)-BKINFO2Q+1)                              
         DS    0CL(BKINFO2Q-(BKINFO2X-BKINFO2L)+1)                              
                                                                                
                                                                                
         DC    AL2(0)                                                           
         EJECT                                                                  
* DAY/TIME INFO                                                                 
*                                                                               
DAYHEAD1 DC    C'Valid day/time formats'                                        
DAYHEAD2 DC    C'----------------------'                                        
*                                                                               
DAYNONE  DC    C'No input in this field is required for '                       
         DC    C'the action you have requested.         '                       
DAYONE   DC    C'One day/time may be input in the follow'                       
         DC    C'ing format -                           '                       
DAYMORE  DC    C'Up to    day/times may be input separat'                       
         DC    C'ed by commas in the following formats -'                       
*                                                                               
DAYINFO  DS    0CL78                                                            
         DC    C'                                       '                       
         DC    C'                                       '                       
         DC    C'Input format is day expression followed'                       
         DC    C' by a ''/'' followed by time expression. '                     
         DC    C'Day abbreviations are M=Mon, TU=Tue, W='                       
         DC    C'Wed, TH=Thu, F=Fri, SA=Sat, SU=Sun.    '                       
         DC    C'     Input single day AS MON, TUE etc. '                       
         DC    C'or use abbreviation.                   '                       
         DC    C'     Input start-end days as M-F, M-SU,'                       
         DC    C'TU-SA etc.                             '                       
         DC    C'     Input rotation schemes as MTWTFSS,'                       
         DC    C' MTW.F.., where ''.'' denotes days not in'                     
         DC    C'                            the rotatio'                       
         DC    C'n scheme.                              '                       
         DC    C'     Input a specific date in the forma'                       
         DC    C't MMMDD (e.g. JAN15, NOV9).            '                       
         DC    C'Time abbreviations are A=AM, P=PM, 12N='                       
         DC    C'noon, 12M=midnight.                    '                       
         DC    C'     Input as start time-end time (e.g.'                       
         DC    C' 6A, 615P, 6-630P, 11A-1P).            '                       
         DC    C'                                       '                       
         DC    C'                                       '                       
         DC    C'Day/time examples - M-F/7-8A, M.W.F../1'                       
         DC    C'2N-1P, MON/7-9P, TU/645P               '                       
         DC    AL1(255)                                                         
         EJECT                                                                  
* DEMO INFO                                                                     
*                                                                               
DEMHEAD1 DC    C'Valid demo formats'                                            
DEMHEAD2 DC    C'------------------'                                            
*                                                                               
DEMNONE  DC    C'No input in this field is required for '                       
         DC    C'the action you have requested.         '                       
DEMONE   DC    C'One demo may be input in the following '                       
         DC    C'formats -                              '                       
DEMMORE  DC    C'Up to   demos may be input (separated b'                       
         DC    C'y commas) in the following formats -   '                       
*                                                                               
DEMINFO  DS    0CL78                                                            
         DC    C'                                       '                       
         DC    C'                                       '                       
         DC    C'Input type                 Input format'                       
         DC    C'                                       '                       
         DC    C'----------                 ------------'                       
         DC    C'                                       '                       
*&&DO                                                                           
         DC    C'ESTIMATE HEADER DEMOS      E=Media/Clie'                       
         DC    C'nt/Product/Estimate (e.g. E=T/CA/PA/1) '                       
         DC    C'SPOTPAK DEMO MENU          M=Media/Menu'                       
         DC    C'-ID (e.g. M=T/23)                      '                       
         DC    C'ALL CATEGORIES             L=demo name '                       
         DC    C'(e.g. L=HOMES, L=WM1849, L=ADULTS)     '                       
         DC    C'DEMO LIST                  Input list o'                       
         DC    C'f demo names or numbers separated by   '                       
         DC    C'                           commas (e.g.'                       
         DC    C' HOMES, RWM1849, 45, P45)              '                       
*&&                                                                             
         DC    AL1(255)                                                         
         EJECT                                                                  
* MORE DEMO INFO                                                                
DMINFO2  DS    0C                                                               
         DC    AL2(DMINFO2X-DMINFO2),AL2(DMINFO2L-DMINFO2)                      
*                                                                               
         DC    AL1(SYSSPT),XL1'00',XL3'00',XL1'00',XL1'00'                      
*                                                                               
DMINFO2L DS    0C                                                               
         DC    C'ESTIMATE HEADER DEMOS      E=Media/Clie'                       
         DC    C'nt/Product/Estimate (e.g. E=T/CA/PA/1) '                       
         DC    C'SPOTPAK DEMO MENU          M=Media/Menu'                       
         DC    C'-ID (e.g. M=T/23)                      '                       
DMINFO2X EQU   *                                                                
DMINFO2Q EQU   ((DMINFO2X-DMINFO2L)/(L'DEMLN1))*L'DEMLN1                        
         DS    0CL((DMINFO2X-DMINFO2L)-DMINFO2Q+1)                              
         DS    0CL(DMINFO2Q-(DMINFO2X-DMINFO2L)+1)                              
                                                                                
                                                                                
DMINFO3  DS    0C                                                               
         DC    AL2(DMINFO3X-DMINFO3),AL2(DMINFO3L-DMINFO3)                      
*                                                                               
         DC    AL1(0),XL1'00',XL3'00',XL1'00',XL1'00'                           
*                                                                               
DMINFO3L DS    0C                                                               
         DC    C'ALL CATEGORIES             L=demo name '                       
         DC    C'(e.g. L=HOMES, L=WM1849, L=ADULTS)     '                       
         DC    C'DEMO LIST                  Input list o'                       
         DC    C'f demo names or numbers separated by   '                       
         DC    C'                           commas (e.g.'                       
         DC    C' HOMES, RWM1849, 45, P45)              '                       
DMINFO3X EQU   *                                                                
DMINFO3Q EQU   ((DMINFO3X-DMINFO3L)/(L'DEMLN1))*L'DEMLN1                        
         DS    0CL((DMINFO3X-DMINFO3L)-DMINFO3Q+1)                              
         DS    0CL(DMINFO3Q-(DMINFO3X-DMINFO3L)+1)                              
                                                                                
                                                                                
         DC    AL2(0)                                                           
         EJECT                                                                  
* DEDEMWRK                                                                      
       ++INCLUDE DEDEMWRK                                                       
         SPACE 1                                                                
* DSECT TO COVER TEMPORARY WORKING STORAGE                                      
*                                                                               
DEMTMPD  DSECT                                                                  
MYRELO   DS    A                                                                
ACTION   DS    XL1                 INPUT ACTION CODE                            
                                                                                
                                                                                
                                                                                
* DSECT TO COVER BOOK INFO CRITERIA                                             
*                                                                               
BKCRITD  DSECT                                                                  
BKCACT   DS    XL1                 DEMO ACTION                                  
BKCFIL   DS    CL3                 FILE                                         
BKCSRC   DS    CL1                 SOURCE                                       
BKCMED   DS    CL1                 MEDIA                                        
BKCRITL  EQU   *-BKCRITD                                                        
                                                                                
                                                                                
                                                                                
* DSECT TO COVER DEMO INFO CRITERIA                                             
*                                                                               
DMCRITD  DSECT                                                                  
DMCSYS   DS    XL1                 SYSTEM                                       
DMCACT   DS    XL1                 DEMO ACTION                                  
DMCFIL   DS    CL3                 FILE                                         
DMCSRC   DS    CL1                 SOURCE                                       
DMCMED   DS    CL1                 MEDIA                                        
DMCRITL  EQU   *-DMCRITD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021DEDEM01   06/16/08'                                      
         END                                                                    
