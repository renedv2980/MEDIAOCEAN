*          DATA SET DELNK10    AT LEVEL 031 AS OF 04/30/10                      
*PHASE TF2F10C                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE TWANG                                                                  
*INCLUDE DUMPOUT                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE UNBOOK                                                                 
*INCLUDE NSIWEEK                                                                
DELNK10  TITLE '- DEMO SYSTEM SERVER'                                           
SVRDEF   CSECT                                                                  
         DC    (RSVRDEFL)X'00'                                                  
         ORG   SVRDEF                                                           
         DC    C'*SERVER**SERVER**SERVER**SERVER*'                              
         DC    AL2(CODE-SVRDEF)    SERVER ENTRY POINT                           
         DC    AL2(FILES-SVRDEF)   SYSTEM/FILE LIST                             
         DC    AL2(FACS-SVRDEF)    FACILITIES LIST                              
         DC    AL2(REQUEST-SVRDEF) REQUEST MAP                                  
         ORG   SVRDEF+(RSVRTYPE-RSVRDEFD)                                       
         DC    C'D'                SERVER TYPE                                  
****     DC    AL1(2)              SPOT SYSTEM                                  
         DC    AL1(8)              REP SYSTEM                                   
         DC    C'DE'               SYSTEM                                       
         DC    C'LK'               PROGRAM                                      
         DC    AL1(WRKIFTWF)       WORKER FILE IN                               
         DC    AL1(WRKIAAPP)       OPEN APPEND                                  
         DC    X'0F2F'             SYSPHASE                                     
         DC    AL1(RSVRILNK+RSVRILCO)                                           
         ORG                                                                    
         EJECT                                                                  
CODE     DS    0D                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**LK10*,CLEAR=YES,RR=RE                                        
         USING LP_D,R1                                                          
         L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         RF=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         DROP  RF                                                               
                                                                                
                                                                                
         OC    RMASTC,RMASTC       TEST RUNNING ONLINE                          
         BNZ   INIT02                                                           
         ICM   R9,15,LP_ABLK1      ROOT SHOULD PASS A(WORKD)                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R8,15,RSVRSAVE      R8=A(4K SAVE AREA)                           
         B     INIT04                                                           
                                                                                
INIT02   L     R9,RSVRSAVE         A(64K SAVE AREA)                             
B#WORKD  EQU   1                   WORKD                                        
         ST    R9,LP_BLKS+((B#WORKD-1)*L'LP_BLKS)                               
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         SR    R8,R8                                                            
         ICM   R8,3,=AL2(WORKL)                                                 
         LA    R8,WORKD(R8)        R8=A(46K SAVE AREA)                          
         USING SAVED,R8,R7         R8=A(SAVE W/S)                               
                                                                                
INIT04   LR    R7,R8                                                            
         AHI   R7,4096                                                          
         ST    R1,ALP              SAVE A(DDLINK PARAMETER LIST)                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         MVC   VERSNUM,LP_VRSN1                                                 
         EJECT                                                                  
***********************************************************************         
* INITIALISE FOR RUNNING                                              *         
***********************************************************************         
                                                                                
RUNSTR   L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         RF=A(RUNPARMS)                               
         CLI   RUNPMODE,RRUNSTRQ   TEST FIRST FOR RUN                           
         BNE   PRCWRK                                                           
         DROP  RF                                                               
                                                                                
         LA    R0,REQVALS          CLEAR DOWN SAVE AREA                         
         LHI   R1,REQVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,RCOMFACS         YES - LOAD FACILITIES OVERLAYS               
         ST    RF,ACOMFACS                                                      
                                                                                
         L     RF,CCALLOV-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,(1,0),0,0                                              
         MVC   AROUT1,0(R1)                                                     
         GOTOR (RF),DMCB,(2,0),0,0                                              
         MVC   AROUT2,0(R1)                                                     
                                                                                
         GOTO1 (RF),DMCB,0,X'D9000A29'       REGETIUN                           
         MVC   VREGTIUN,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A24'       SPGETIUN                           
         MVC   VSPGTIUN,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000A00'       BOOKVAL                            
         MVC   VBOOKVAL,0(R1)                                                   
         GOTO1 (RF),(R1),0,X'D9000AE0'       DEMOCON                            
         MVC   VDEMOCON,0(R1)                                                   
                                                                                
         GOTOR (#WRKINI,AWRKINI)   INITIALISE WORKING STORAGE                   
                                                                                
         L     R1,ALP                                                           
         MVC   LP_AUIR1,AROUT1     SET A(INDEX ROUTINES 1)                      
         MVC   LP_AUIR2,AROUT2     SET A(INDEX ROUTINES 2)                      
                                                                                
RUNSTR02 L     R1,ALP                                                           
         LA    R0,SAVED                                                         
B#SAVED  EQU   2                   SAVED                                        
         ST    R0,LP_BLKS+((B#SAVED-1)*L'LP_BLKS)                               
                                                                                
         J     EXITY                                                            
                                                                                
***********************************************************************         
* FIRST FOR NEW WORK                                                  *         
***********************************************************************         
                                                                                
PRCWRK   L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         RF=A(RUNPARMS)                               
         CLI   RUNPMODE,RPRCWRKQ   TEST 'PROCESS WORK' MODE                     
         BNE   RUNREQ                                                           
         DROP  RF                                                               
         XC    REQVALS(REQVALSL),REQVALS                                        
*****    GOTOR RVALPAR,=CL80'TRACE=B'                                           
                                                                                
         L     RE,=V(DUMPOUT)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,VDUMPOUT                                                      
                                                                                
         LA    R0,REQVALS          CLEAR DOWN SAVE AREA                         
         LHI   R1,REQVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         J     EXITY                                                            
                                                                                
*=====================================================================*         
* RUN A DOWNLOAD REQUEST                                              |         
*=====================================================================*         
                                                                                
RUNREQ   L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         RF=A(RUNPARMS)                               
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         JNE   EXITY                                                            
         DROP  RF                                                               
*                                                                               
         L     RF,ALP                                                           
         L     RF,LP_ARUNP-LP_D(RF)                                             
         L     RF,RUNPMODE-RUNPARMD(RF)                                         
         L     RF,RMASTC-RUNFACSD(RF)                                           
         MVC   DEPRINT,MCVPRINT-MASTD(RF)      ADDRESS OF PRINT ROUT            
                                                                                
         CLC   LP_QMAPN,=AL2(M#INIT)                                            
         JE    RUNINIT                                                          
         CLC   LP_QMAPN,=AL2(M#STATL)                                           
         JE    RUNSTAT                                                          
         CLC   LP_QMAPN,=AL2(M#MRKTL)                                           
         JE    RUNMRKT                                                          
         CLC   LP_QMAPN,=AL2(M#VALSTA)                                          
         JE    RUNVSTA                                                          
         CLC   LP_QMAPN,=AL2(M#VALSBK)                                          
         JE    RUNVSTBK                                                         
         CLC   LP_QMAPN,=AL2(M#VALDTM)                                          
         JE    RUNVDYTM                                                         
         CLC   LP_QMAPN,=AL2(M#VALDEM)                                          
         JE    RUNVDEMO                                                         
         CLC   LP_QMAPN,=AL2(M#SPILL)                                           
         JE    RUNSPILL                                                         
         CLC   LP_QMAPN,=AL2(M#BOOKL)                                           
         JE    RUNBOOKL                                                         
         CLC   LP_QMAPN,=AL2(M#DEMOL)                                           
         JE    RUNDEMOL                                                         
         CLC   LP_QMAPN,=AL2(M#VALUP)                                           
         JE    RUNVUPGD                                                         
         J     EXITY                                                            
                                                                                
RUNSTAT  GOTOR =A(STATLIST),RR=SRVRRELO            STATION LIST                 
         J     EXITY                                                            
RUNMRKT  GOTOR =A(MRKTLIST),RR=SRVRRELO            MARKET LIST                  
         J     EXITY                                                            
RUNVSTA  GOTOR =A(VALSTATL),RR=SRVRRELO            VALIDATED STATION            
         J     EXITY                               LIST                         
RUNVSTBK GOTOR =A(VBKLIST),RR=SRVRRELO             VALIDATED BOOK LIST          
         J     EXITY                                                            
RUNVDYTM GOTOR =A(VDYTLIST),RR=SRVRRELO            VALIDATED DAYTIME            
         J     EXITY                               LIST                         
RUNVDEMO GOTOR =A(VDEMLIST),RR=SRVRRELO            VALIDATED DEMO LIST          
         J     EXITY                                                            
RUNSPILL GOTOR =A(SPILLIST),RR=SRVRRELO            SPILL LIST                   
         J     EXITY                                                            
RUNBOOKL GOTOR =A(BOOKLIST),RR=SRVRRELO            BOOK LIST                    
         J     EXITY                                                            
RUNDEMOL GOTOR =A(DEMOLIST),RR=SRVRRELO            DEMO LIST                    
         J     EXITY                                                            
RUNVUPGD GOTOR =A(VALUPGD),RR=SRVRRELO             VALIDATE UPGRADE             
         J     EXITY                                                            
                                                                                
**********************************************************************          
*                                                                    *          
*     INITIAL DOWNLOAD CODE                                          *          
*                                                                    *          
**********************************************************************          
                                                                                
RUNINIT  DS    0H                                                               
                                                                                
         L     RE,=A(FILETAB)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,FILEPTR                                                       
         L     RE,=A(SRCETAB)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,SRCEPTR                                                       
         L     RE,=A(ASETIMES)                                                  
         A     RE,SRVRRELO                                                      
         ST    RE,SETIMPTR                                                      
         L     RE,=A(ABKTPCDE)                                                  
         A     RE,SRVRRELO                                                      
         ST    RE,BKTYPPTR                                                      
         L     RE,=A(OSPWKTAB)                                                  
         A     RE,SRVRRELO                                                      
         ST    RE,OSPWKPTR                                                      
         L     RE,=A(OMBKTAB)                                                   
         A     RE,SRVRRELO                                                      
         ST    RE,OMBKPTR                                                       
* PASS DOWN TO PC THE SIZE OF BUFFERS AVAILABLE PER TRANASACTION FOR            
* REPORT                                                                        
         MVI   MAXNDEM,14                                                       
         MVI   MAXBKSTN,125                                                     
         LA    RF,BUFFVALS                                                      
         USING BUFFVALS,RF                                                      
         MVI   BUFFSEND,C'Y'                                                    
         MVC   BUFFNUM,=X'0001'                                                 
         MVC   BUFFTOTS,=A(CMPMAXSZ)                                            
         MVC   BUFFSUMM,=A(CMPBKFIL)     FILBKL                                 
*                                                                               
* ADD 2 TO BUFFSTAS WHEN WE SUPPORT SYSCODE  -STATTABL IN DELNK11               
         MVC   BUFFSTAS,=A(CMPSTADT)     EACH STATION/DAYTIME                   
*                                                                               
         MVC   BUFFDEMO,=A(CMPDEMOS)                                            
* REMEMBER I HAVE ALREADY SUPPORTED A DECIMAL MAPCDE AND STORAGE                
* IN DELNK11 - IF WE NEED TO SUPPORT HIS IN COMPARAGRAPH ITSELF                 
* TO ADD 1 BYTE FOR THE DECIMAL PRECISION FIELD HERE                            
         MVC   BUFFPROJ,=A(CMPPROJ)                                             
         MVC   BUFFVBKS,=A(CMPVALBK)     EACH ELEMENT OF VALIDATION             
         MVC   BUFFVSTA,=A(CMPVSTAT)     VALIDATE STAT BUFF                     
         MVC   BUFFPRJN,=A(CMPNPROJ)     MAX # OF PRJS ALLOWED                  
         MVC   BUFFPAVL,=A(CMAXPAVR)     PAV UPGRADE RECAL LIMIT                
         MVC   PROJFLAG,PROJREC                                                 
         DROP  RF                        BOOKS FOR STATION                      
                                                                                
         MVC   DUB(4),=C'RRMP'                                                  
         XC    DMCB,DMCB                                                        
         LA    RF,DUB                                                           
         ST    RF,DMCB                                                          
         GOTOR (#GETPROF,AGETPROF),DMCB                                         
                                                                                
         LA    RF,PROFVALS                                                      
         USING PROFVALS,RF                                                      
         MVI   PROFSEND,C'Y'                                                    
         MVC   PROFNUM,=X'0001'                                                 
         MVI   PROFHIST,C'N'                                                    
         MVI   PROFSHPT,C'N'                                                    
         MVI   PROFPJFM,C'N'                                                    
         TM    PROFRRMP,X'20'                                                   
         BZ    *+8                                                              
         MVI   PROFHIST,C'Y'                                                    
         TM    PROFRRMP+2,X'40'                                                 
         BZ    *+8                                                              
         MVI   PROFSHPT,C'Y'                                                    
         TM    PROFRRMP+2,X'10'                                                 
         BZ    *+8                                                              
         MVI   PROFPJFM,C'Y'                                                    
         DROP  RF                                                               
                                                                                
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
RUN10    L     RE,FILEPTR                                                       
         OC    0(2,RE),0(RE)                                                    
         JZ    RUN420                                                           
                                                                                
         LA    RF,FSRCVALS                                                      
         USING FSRCVALS,RF                                                      
         L     RE,FILEPTR                                                       
         MVC   FILECODE,0(RE)                    FILECODE                       
         MVC   FILEDIS,3(RE)                     FILE DESCRIPTION               
         AHI   RE,L'FILETAB                                                     
         ST    RE,FILEPTR                                                       
         CLC   =C'PAV',FILECODE                  PAV NOT AVAILABLE              
         JNE   *+12                                                             
         CLI   OVSYS,8                           FOR NON REP SYSTEM             
         JNE   RUN300                            REP?                           
         CLC   =C'INV',FILECODE                  INV NOT AVAILABLE              
         JNE   *+12                                                             
         CLI   OVSYS,8                           FOR NON REP SYSTEM             
         JNE   RUN300                            REP?                           
                                                                                
         L     RE,SRCEPTR                                                       
         MVC   SRCECODE,0(RE)                    SOURCE CODE                    
         MVC   SRCEDIS,5(RE)                     SOURCE DESCRIPTION             
         AHI   RE,L'SRCETAB                                                     
         ST    RE,SRCEPTR                                                       
         MVC   FSRCENUM,=H'1'                                                   
         DROP  RF                                                               
                                                                                
         L     R3,BKTYPPTR                                                      
         L     R3,0(R3)                                                         
         A     R3,SRVRRELO                                                      
         L     RE,BKTYPPTR                                                      
         AHI   RE,4                                                             
         ST    RE,BKTYPPTR                                                      
         LA    RF,BKTYVALS                                                      
         USING BKTYVALS,RF                                                      
         LA    RE,0                                                             
RUN100   OC    0(2,R3),0(R3)                                                    
         JZ    RUN120                                                           
         MVC   BOOKTYPE,0(R3)                                                   
         MVC   BKTYDISP,5(R3)                                                   
         AHI   R3,L'BKTPCDE1                                                    
         AHI   RF,BKTYVALL                                                      
         AHI   RE,1                                                             
         J     RUN100                                                           
         DROP  RF                                                               
                                                                                
RUN120   DS    0H                                                               
         STCM  RE,3,BKTYPNUM                                                    
                                                                                
         CLC   =C'INV',FILECODE                                                 
         BE    RUN285                                                           
         L     R3,SETIMPTR                                                      
         L     R3,0(R3)                                                         
         A     R3,SRVRRELO                                                      
                                                                                
         L     RE,SETIMPTR                                                      
         AHI   RE,4                                                             
         ST    RE,SETIMPTR                                                      
                                                                                
         LA    RF,SETMVALS                                                      
         USING SETMVALS,RF                                                      
         LA    RE,0                                                             
RUN140   OC    0(2,R3),0(R3)                                                    
         JZ    RUN220                                                           
         MVC   STIME,0(R3)                                                      
         MVC   ETIME,2(R3)                                                      
         MVC   INTVL,4(R3)                                                      
         AHI   R3,L'SETIMES1                                                    
         AHI   RF,SETMVALL                                                      
         AHI   RE,1                                                             
         J     RUN140                                                           
         DROP  RF                                                               
RUN220   STCM  RE,3,SETIMNUM                                                    
         L     R3,=A(DYTMTAB)                                                   
         A     R3,SRVRRELO                                                      
         LA    RF,DYTMVALS                                                      
         USING DYTMVALS,RF                                                      
         LA    RE,0                                                             
RUN240   OC    0(2,R3),0(R3)                                                    
         JZ    RUN280                                                           
         MVC   DYTIMCDE,0(R3)                                                   
         MVC   DYTMDISP,6(R3)                                                   
         MVI   DYTMSEND,YESQ                                                    
         AHI   R3,L'DYTMTAB                                                     
         AHI   RF,DYTMVALL                                                      
         AHI   RE,1                                                             
         J     RUN240                                                           
RUN280   STCM  RE,3,DYTIMNUM                                                    
         DROP  RF                                                               
                                                                                
                                                                                
RUN285   L     R3,=A(DMMODTAB)                                                  
         A     R3,SRVRRELO                                                      
         LA    RF,DMODVALS                                                      
         USING DMODVALS,RF                                                      
         LA    RE,0                                                             
RUN290   OC    0(2,R3),0(R3)                                                    
         JZ    RUN294                                                           
         MVC   DEMOMOD,0(R3)                                                    
         MVC   DMMODNAM,5(R3)                                                   
         MVI   DMODSEND,YESQ                                                    
         AHI   R3,L'DMMODTAB                                                    
         AHI   RF,DMODVALL                                                      
         AHI   RE,1                                                             
         J     RUN290                                                           
                                                                                
                                                                                
                                                                                
RUN294   STCM  RE,3,DMODNUM                                                     
         L     RE,OSPWKPTR                                                      
         MVC   OPTSPWKS,0(RE)                                                   
         AHI   RE,1                                                             
         ST    RE,OSPWKPTR                                                      
         L     RE,OMBKPTR                                                       
         MVC   OPTMBKS,0(RE)                                                    
         AHI   RE,1                                                             
         ST    RE,OMBKPTR                                                       
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
RUN300   GOTOR =A(CLRAREA),RR=SRVRRELO                                          
         J     RUN10                             NEXT FILE SRC COMBO            
RUN420   DS    0H                                                               
         CLI   OVSYS,8                                                          
         JNE   EXITY                             REP?                           
         GOTOR =A(READDYPT),RR=SRVRRELO          READ DAYPARTS                  
         J     EXITY                                                            
         DROP  RF                                                               
         EJECT                                                                  
         DROP  R1                                                               
         LTORG                                                                  
         DROP  RB                                                               
OSPWKPTR DS    A                                                                
OMBKPTR  DS    A                                                                
                                                                                
PROJREC  DC    CL19'YYNYNNNYYYNNYYYYNYY'                                        
FILETAB  DS    0CL(3+25)                                                        
         DC    CL3'TT ',CL25'TYPICAL TIME PERIOD      '                         
         DC    CL3'T4 ',CL25'4 WEEK TIME PERIOD       '                         
         DC    CL3'PAV',CL25'PROGRAM AVERAGE          '                         
         DC    CL3'INV',CL25'INVENTORY                '                         
***      DC    CL3'WTP',CL25'WEEKLY METER             '                         
         DC    AL2(0)                                                           
SRCETAB  DS    0CL(5+21)                                                        
         DC    CL5'NSI  ',CL21'NIELSON              '                           
         DC    CL5'NSI  ',CL21'NIELSON              '                           
         DC    CL5'NSI  ',CL21'NIELSON              '                           
         DC    CL5'INV  ',CL21'INVENTORY            '                           
***      DC    CL5'NSI  ',CL21'NIELSON              '                           
         DC    AL2(0)                                                           
ABKTPCDE DC    AL4(BKTPCDE1)                     TP                             
         DC    AL4(BKTPCDE1)                     T4                             
         DC    AL4(BKTPCDE2)                     PAV                            
         DC    AL4(BKTPCDE1)                     INV                            
***      DC    AL4(BKTPCDE3)                     WTP                            
BKTPCDE1 DS    0CL(5+30)                                                        
         DC    CL5'     ',CL30'Standard Survey               '                  
         DC    CL5'B    ',CL30'Black                         '                  
         DC    CL5'H    ',CL30'Hispanic                      '                  
         DC    CL5'C    ',CL30'Cable                         '                  
         DC    CL5'M    ',CL30'Metro                         '                  
         DC    CL5'T    ',CL30'Trading area                  '                  
         DC    CL5'O    ',CL30'Olympic Excl.                 '                  
         DC    CL5'E    ',CL30'Extended                      '                  
         DC    CL5'D    ',CL30'DMA Prv/Spcl                  '                  
         DC    CL5'N    ',CL30'Special FoxNet                '                  
         DC    CL5'P    ',CL30'People Meter                  '                  
         DC    CL5'X    ',CL30'Misc Exclusions               '                  
         DC    CL5'A    ',CL30'Parent Only                   '                  
         DC    CL5'I    ',CL30'Hispanic LPM                  '                  
         DC    CL5'W    ',CL30'Hardwired Cable               '                  
         DC    CL5'Y    ',CL30'NSI LPM Hardwired Cable       '                  
         DC    CL5'1    ',CL30'Standard 0 Cell               '                  
         DC    CL5'2    ',CL30'Hispanic 0 Cell               '                  
         DC    CL5'3    ',CL30'Cable 0 Cell                  '                  
         DC    CL5'4    ',CL30'Hardwired 0 Cell              '                  
         DC    CL5'L    ',CL30'Live                          '                  
         DC    CL5'L3   ',CL30'Live+3                        '                  
         DC    CL5'LS   ',CL30'Live+SD                       '                  
         DC    CL5'J    ',CL30'Live Hispanic                 '                  
         DC    CL5'HS   ',CL30'Live+SD Hispanic              '                  
         DC    CL5'U    ',CL30'Live Cable                    '                  
         DC    CL5'Z    ',CL30'Live Hardwired                '                  
         DC    CL5'W3   ',CL30'Live+3 Hardwired              '                  
         DC    CL5'WS   ',CL30'Live+SD Hardwired             '                  
         DC    CL5'SS   ',CL30'Live+SD hardwired Parent Only '                  
         DC    CL5'QS   ',CL30'Live+SD Parent Only           '                  
MAXBTNUM EQU   (*-BKTPCDE1)/L'BKTPCDE1                                          
         DC    AL2(0)                                                           
BKTPCDE2 DS    0CL(5+30)                                                        
         DC    CL5'     ',CL30'Standard Survey               '                  
         DC    CL5'B    ',CL30'Black                         '                  
         DC    CL5'H    ',CL30'Hispanic                      '                  
         DC    CL5'M    ',CL30'Metro                         '                  
         DC    CL5'T    ',CL30'Trading Area                  '                  
         DC    CL5'O    ',CL30'Olympic Excl.                 '                  
         DC    CL5'E    ',CL30'Extended                      '                  
         DC    CL5'D    ',CL30'DMA Prv/Spcl                  '                  
         DC    CL5'N    ',CL30'Special FoxNet                '                  
         DC    CL5'P    ',CL30'People Meter                  '                  
         DC    CL5'X    ',CL30'Misc Exclusions               '                  
         DC    CL5'I    ',CL30'Hispanic LPM                  '                  
         DC    CL5'L    ',CL30'Live                          '                  
         DC    CL5'L3   ',CL30'Live+3                        '                  
         DC    CL5'LS   ',CL30'Live+SD                       '                  
         DC    CL5'J    ',CL30'Live Hispanic                 '                  
         DC    CL5'HS   ',CL30'Live+SD Hispanic              '                  
         DC    CL5'U    ',CL30'Live Cable                    '                  
         DC    CL5'Z    ',CL30'Live Hardwired                '                  
         DC    CL5'Z3   ',CL30'Live+3 Hardwrired             '                  
         DC    CL5'WS   ',CL30'Live+SD Hardwrired            '                  
         DC    CL5'SS   ',CL30'Live+SD Hardwired Parent Only '                  
         DC    CL5'QS   ',CL30'Live+SD Parent Only           '                  
         DC    AL2(0)                                                           
*&&DO                                                                           
BKTPCDE3 DS    0CL(5+30)                                                        
         DC    CL5'     ',CL30'Standard Survey               '                  
         DC    AL2(0)                                                           
*&&                                                                             
ASETIMES DC    AL4(SETIMES1)                     TP                             
         DC    AL4(SETIMES1)                     T4                             
         DC    AL4(SETIMES2)                     PAV                            
**       DC    AL4(SETIMES1)                     WTP                            
SETIMES1 DS    0XL(2+2+1)                                                       
         DC    AL2(0500,0500+2400),AL1(30)       NSI TP                         
         DC    AL2(0)                                                           
SETIMES2 DS    0XL(2+2+1)                                                       
         DC    AL2(0600,0200+2400),AL1(30)       NSI PAV                        
         DC    AL2(0)                                                           
                                                                                
DYTMTAB  DC    0XL(6+25)                                                        
         DC    CL6'M-F   ',CL25'M - FRI AVG              '                      
         DC    CL6'M-SU  ',CL25'M - SU AVG               '                      
         DC    CL6'SA-SU ',CL25'SA - SU AVG              '                      
         DC    CL6'VAR   ',CL25'VARIOUS ROTATION         '                      
         DC    CL6'AVG2  ',CL25'2-DAY AVERAGE ROTATION   '                      
         DC    CL6'AVG3  ',CL25'3-DAY AVERAGE ROTATION   '                      
         DC    CL6'AVG4  ',CL25'4-DAY AVERAGE ROTATION   '                      
         DC    CL6'AVG5  ',CL25'5-DAY AVERAGE ROTATION   '                      
         DC    CL6'AVG6  ',CL25'6-DAY AVERAGE ROTATION   '                      
         DC    AL2(0)                                                           
                                                                                
                                                                                
DMMODTAB DC    0CL(5+25)                                                        
         DC    CL5'R    ',CL25'Ratings                  '                       
         DC    CL5'D    ',CL25'Market Impressions       '                       
         DC    CL5'     ',CL25'Impressions              '                       
         DC    AL2(0)                                                           
                                                                                
OSPWKTAB DC    0CL4                              OPTION SPECIFY WEEKS           
         DC    C'YNYN'                                                          
         DC    AL2(0)                                                           
OMBKTAB  DC    0CL4                              OPTION MULTI BOOK AVG          
         DC    C'YYNN'                                                          
         DC    AL2(0)                                                           
                                                                                
EXITN    CR    RB,RE                                                            
EXITY    CR    RE,RE                                                            
EXIT     XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
YESQ     EQU   C'Y'                                                             
SPSYSQ   EQU   2                                                                
                                                                                
***********************************************************************         
* LIST OF MEDIA FILES TO OPEN IN ALL SYSTEMS                          *         
***********************************************************************         
                                                                                
FILES    DS    0X                                ** FILE INFO **                
         DC    C'SPOT   '                        SYSTEM NAME FOR OPEN           
*****    DC    C'REP    '                        SYSTEM NAME FOR OPEN           
                                                                                
         DC    C'NDEMDIRA'                                                      
         DC    C'NDEMDIRN'                                                      
         DC    C'NDEMDIRR'                                                      
         DC    C'NL=DEMFA'                                                      
         DC    C'NL=DEMFN'                                                      
         DC    C'NL=DEMFR'                                                      
         DC    C'NNTIDIR '                                                      
         DC    C'NL=NTIFL'                                                      
         DC    C'NPAVDIR '                                                      
         DC    C'NL=PAVFL'                                                      
         DC    C'NCTFILE '                                                      
***      DC    C'NREPDIR '                                                      
***      DC    C'NREPFIL '                                                      
         DC    C'X'                                                             
         EJECT                                                                  
***********************************************************************         
* LIST OF CORE RESIDENT FACILITIES (MAPS TO SYSADDR IN WORKD)         *         
***********************************************************************         
FACS     DS    0X                                                               
         DC    AL1(QDEMAND),AL2(CDEMAND-COMFACSD,VDEMAND-SYSADDR)               
         DC    AL1(QDEMOUT),AL2(CDEMOUT-COMFACSD,VDEMOUT-SYSADDR)               
         DC    AL1(QDEMAINT),AL2(CDEMAINT-COMFACSD,VDEMAINT-SYSADDR)            
         DC    AL1(QDEFINE),AL2(CDEFINE-COMFACSD,VDEFINE-SYSADDR)               
         DC    AL1(0),AL2(CDATAMGR-COMFACSD,VDATAMGR-SYSADDR)                   
         DC    AL1(QDEMADDR),AL2(CDEMADDR-COMFACSD,0)                           
         DC    AL1(0),AL2(CSWITCH-COMFACSD,VSWITCH-SYSADDR)                     
FACSX    DC    AL1(0)                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
*                                                                     *         
*   GET STATIONS FOR MARKET                                           *         
*                                                                     *         
***********************************************************************         
                                                                                
STATLIST NTR1  BASE=*                                                           
                                                                                
         XC    PREVSTMK,PREVSTMK                                                
         CLC   VERSNUM,=AL4(VERS48)                                             
         BH    STATL10                                                          
         MVI   DMCB,5                                                           
         MVC   DMCB+1(2),=X'0005'                                               
         B     STATL11                                                          
                                                                                
STATL10  MVI   DMCB,6                                                           
         MVC   DMCB+1(2),=X'0006'                                               
                                                                                
STATL11  MVC   ATSIOREC,AIO3                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
         MVC   DBFIL,=C'TP '                     TIME PERIOD                    
         MVI   DBSRC,C'N'                        NIELSON                        
         MVI   DBMED,C'T'                        USTV                           
* THIS SHOULD COME OUT, WE DONT EVEN READ THE MARKET NAMES IN THE               
* STATION LISTING.  NO POINT OF HAVING THIS CALL                                
*&&DO                                                                           
         LA    R3,KEY                            BUILD KEY FOR MARKET           
         USING DMKEY,R3                          NAME RECORD R3=A(KEY)          
         XC    DMKMAJOR,DMKMAJOR                                                
         MVI   DMCODE,DMCODEQU                                                  
         MVC   DMMEDIA,DBMED                                                    
         MVC   DMSRC,DBSRC                                                      
         MVI   IOFLAG,DIR+READ+DEM                                              
         GOTOR =A(IO),RR=SRVRRELO                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
* -----------------------------------------                                     
*  GET STATION AND TRANSLATE TO NUMERIC   |                                     
* -----------------------------------------                                     
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,AAMKT                                                       
         LA    R5,1                                                             
         TM    AMKTIND,LQ_TSINQ                                                 
         BNO   STATL12                                                          
         LA    RE,LW_DATA1-LW_D(RE)                                             
         B     STATL14                                                          
STATL12  TM    AMKTIND,LQ_TLSTQ                                                 
         BO    *+6                                                              
         DC    H'0'                                                             
         ICM   R5,3,LW_NUMN-LW_D(RE)             GET NUMBER OF ENTRIES          
         LA    RE,LW_DATA2-LW_D(RE)              GET DATA IN WORK POOL          
STATL14  MVC   DUB(3),2(RE)                      GET MARKET INPUTED             
         MVC   KMARKET,0(RE)                                                    
         OC    KMARKET,KMARKET                   IF WE HAVE BINARY MKT          
         JNZ   STATL16                                                          
         MVC   DUB(3),2(RE)                      GET MARKET INPUTTED            
         BAS   RE,TRAMKT                                                        
         OC    DUB+3(2),DUB+3                    DUB+3(2) HAS BINARY            
         JZ    EXITY                             MKT#, IF NO MKT# THEN          
         MVC   KMARKET,DUB+3                     DONT DO ANYTHING               
STATL16  XC    BKS,BKS                                                          
STATL18  LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                        R5=A(DBLOCK)                   
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIO2                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,DBFIL                                                     
         MVI   DBFUNCT,DBGETASB                 GET ALL BOOKS ALL STAT          
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELBK,BKS                                                      
         MVC   DBSELRMK,KMARKET                                                 
                                                                                
         XC    PREVSTTN,PREVSTTN                                                
         XC    STATNUM,STATNUM                                                  
         LA    RE,STATVALS                                                      
         ST    RE,STATPTR                                                       
                                                                                
         GOTOR (#SETUSID,ASETUSID)               CALL DEMAND TO                 
         GOTO1 VDEMAND,DMCB,DBLOCKD,STATLHK,0    READ RECORDS                   
         J     EXITY                                                            
         EJECT                                                                  
**       DROP  R3                                                               
                                                                                
* -------------------------------------------------------------                 
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND       |                 
* -------------------------------------------------------------                 
                                                                                
STATLHK  ST    RE,SAVERE                         SAVE RETURN ADDRESS            
                                                                                
         CLC   STATNUM,=H'4000'                  TESTING                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   MARNAME,SPACES                                                   
         MVC   MARNAME(11),=C'**UNKNOWN**'                                      
         LA    R1,DBKEY                                                         
         USING MLKEY,R1                                                         
STHOOK2  CLI   DBRECTYP,DBRECMS                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MARKET,MLRMKT                     EXTRACT MARKET                 
         MVC   KMARKET,MLKMKT                    AND STATION                    
         MVC   STACALL,MLSTAT                                                   
         MVC   KBOOK,MLBOOK                                                     
         XC    KBOOK,=X'FFFF'                                                   
         MVC   KBTYP,MLBTYP                                                     
                                                                                
         CLC   KBOOK,=X'5A07'                    IF BK IS PRIOR TO              
         BNL   *+8                               BOOKTYPE CUTOFF                
         MVI   KBTYP,0                           KILL THE BOOKTYPE              
         TM    STACALL,X'F0'                     NOT INTERESTED IN              
         BO    STHOOKX                           MARKET SPILL                   
         CLI   DBMED,C'T'                        IF MEDIA IS USTV,              
         BNE   STHOOK4                                                          
         CLI   KBTYP,X'05'                       IF IT'S NSI 2A-6A DATA         
         BE    STHOOKX                           IGNORE THIS RECORD             
         CLI   KBTYP,C'C'                        AND BKTYPE IS CABLE            
         BNE   STHOOK4                                                          
                                                                                
STHOOK4  CLI   DBSELMED,C'N'                                                    
         BE    STHOOK8                                                          
         CLI   DBMED,C'T'                        IF MEDIA IS USTV,              
         BNE   STHOOK6                                                          
         CLI   DBSRC,C'N'                        AND SOURCE IS NIELSEN,         
         BNE   STHOOK6                                                          
         CLI   KBTYP,C'C'                        AND BOOKTYPE IS CABLE,         
         BNE   STHOOK6                                                          
         B     STHOOK8                           SKIP STATION SPILL             
STHOOK6 EQU    *                                                                
         MVI   SPILLFLG,C'N'                                                    
         OC    MLKMKT,MLKMKT                     TEST IS SPILL STATION          
         BZ    STHOOK8                                                          
*    SPILL STAT TURN ON SPILL FLAG                                              
         MVI   SPILLFLG,C'Y'                                                    
         DROP  R1                                                               
                                                                                
STHOOK8  DS    0H                                GET SPILL MARKET FROM          
         LHI   R5,DBLOCK1-WORKD                  SPILL STATION                  
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                                                       
         MVC   MARRALF,SPACES                                                   
         CLI   DBSELMED,C'N'                                                    
         BE    STHOOK14                                                         
         SR    R0,R0                             SAVE MKT NUMBERS IN R0         
         ICM   R0,12,DBSELMK                     H.O.2.B =SPILL MARKET#         
         ICM   R0,3,DBSELRMK                     L.O.2.B =RTG SVCE MKT#         
         XC    DBSELMK,DBSELMK                                                  
         MVC   DBSELRMK,MARKET                                                  
         MVC   WORK,SPACES                                                      
                                                                                
STHOOK10 DS    0H                                                               
         OC    KMARKET,KMARKET                                                  
         BZ    STHOOK12                                                         
***      GOTOR VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK,RR=SRVRRELO                   
         GOTO1 VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK                               
         MVC   MARRALF,WORK                                                     
STHOOK12 STCM  R0,12,DBSELMK                     RESTORE SPILL MARKET#          
         STCM  R0,3,DBSELRMK                     RESTORE RTG SVCE MKT#          
         DROP  R5                                                               
                                                                                
         MVI   XSPLLFLG,C'N'                                                    
         CLI   KBTYP,X'E0'                       IF XTRA SPILL                  
         BNE   *+12                                                             
         MVI   KBTYP,0                                                          
         MVI   XSPLLFLG,C'Y'                                                    
* CHECK TO SEE IF RECORD EXIST                                                  
         XC    WORK,WORK                         3 BYTE SEARCH KEY              
         MVC   WORK(5),STACALL                                                  
                                                                                
         CLC   VERSNUM,=AL4(VERS48)                                             
         BNH   *+10                                                             
         MVC   WORK+5(1),KBTYP                                                  
                                                                                
         MVC   WORK2,WORK                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#READTSR,AREADTSR),DMCB                                         
         CLI   DMCB,C'Y'                                                        
         BE    STHOOKX                                                          
         MVC   WORK,WORK2                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#WRITTSR,AWRITTSR),DMCB                                         
                                                                                
STHOOK13 MVC   BKS(2),KBOOK                                                     
         MVC   BKS+2(1),KBTYP                                                   
*                                                                               
         OC    PREVSTMK,PREVSTMK          DID WE ALREADY HAVE AFF               
         BZ    STHK13B                                                          
         CLC   STACALL,PREVSTMK                                                 
         BNE   STHK13B                                                          
         CLC   KMARKET,PREVSTMK+5                                               
         BE    STHOOK14                                                         
STHK13B  MVC   PREVSTMK(5),STACALL                                              
         MVC   PREVSTMK+5(2),KMARKET                                            
         BAS   RE,GETAFFL                                                       
STHOOK14 DS    0H                                                               
                                                                                
         LA    R4,STATVALS                                                      
         USING STATVALS,R4                                                      
         XC    STATION,STATION                                                  
         MVC   STATION(L'STACALL),STACALL                                       
         OC    STATION,SPACES                                                   
         CLI   STATION+4,C'T'                                                   
         BNE   *+8                                                              
         MVI   STATION+4,C' '                                                   
         OC    KMARKET,KMARKET                                                  
         BZ    STHOOK16                                                         
         LA    RE,STATION+2                                                     
         CLI   STATION+2,C' '                                                   
         BNE   *+12                                                             
         LA    RE,STATION+2                                                     
         B     STHOOK15                                                         
         LA    RE,STATION+3                                                     
         CLI   STATION+3,C' '                                                   
         BE    *+8                                                              
         LA    RE,STATION+4                                                     
STHOOK15 MVI   0(RE),C'/'                                                       
         MVC   1(L'MARRALF,RE),MARRALF                                          
STHOOK16 MVC   STATAFFL,AFFILS                                                  
         MVI   STATSEND,YESQ                                                    
         XC    STATFLAG,STATFLAG                                                
                                                                                
         CLI   DBSRC,C'N'                        IF NIELSEN,                    
         BNE   STHOOK18                                                         
         CLI   DBMED,C'T'                        USTV,                          
         BNE   STHOOK18                                                         
         CLI   KBTYP,C'C'                        &  BOOKTYPE IS "CABLE"         
         BNE   STHOOK18                                                         
         B     STHOOK20                          DONT FLAG AS SPILL             
                                                                                
STHOOK18 OC    KMARKET,KMARKET                                                  
         BZ    *+8                                                              
         OI    STATFLAG,SPILLEQU                 SPILL STATION ?                
STHOOK20 CLC   =C'P ',AFFILS                                                    
         BNE   *+8                                                              
         OI    STATFLAG,PUBEQU                   PUBLIC STATION?                
         CLI   KBTYP,C'C'                                                       
         BNE   *+8                                                              
         OI    STATFLAG,CABLEEQU                 CABLE STATION ?                
         DROP  R4                                                               
                                                                                
         MVC   STATBKTY,KBTYP                    BOOK TYPE,                     
*                                                                               
*TRANSLATE BOOKTYPE TO 2 BYTE FORMAT                                            
         GOTOR  (#TRNSBKT,ATRNSBKT),DMCB,KBTYP,1,STATBKTY,12                    
         CLI   BKS+2,X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   BKS+2(2),=C'??'       SEND DOWN ?? AS BOOKTYPE                   
         MVC   HALF1,STATNUM                                                    
         MVC   STATNUM,=X'0001'                                                 
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         MVC   STATNUM,HALF1                                                    
                                                                                
STHOOKX  L     RE,SAVERE                         RETURN TO DEMAND               
         BR    RE                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
GETAFFL  NTR1                                                                   
*   ALPHA MKT MARKET STILL MUCKED UP- KEEP ON FUDGING                           
         MVC   AFFILS,SPACES                                                    
         CLI   DBSRC,C'F'                                                       
         BE    GAFX                                                             
         CLI   KBTYP,BOOKTYPE_S3                 CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,BOOKTYPE_C3                 CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,BOOKTYPE_W3                 CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,C'C'                        CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,C'W'                        CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,C'U'                        CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,C'Z'                        CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,C'3'                        CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,C'4'                        CABLE DONT HAVE                
         BE    GAFX                              AFFILIATES                     
         CLI   KBTYP,C'D'                        CABLE DONT HAVE                
         BE    *+8                               AFFILIATES                     
         CLI   KBTYP,C'P'         SKIP AFFILIATES LOOKUP FOR CABLES             
         BE    *+8                                                              
         CLI   KBTYP,C'B'         SKIP AFFILIATES LOOKUP FOR CABLES             
         BE    *+8                                                              
         CLI   KBTYP,C'H'         SKIP AFFILIATES LOOKUP FOR CABLES             
         BE    *+8                                                              
         CLI   KBTYP,C'J'                                                       
         BNE   *+14                                                             
         OC    KMARKET,KMARKET    CHECK SPILL                                   
         BNZ   GAFX                                                             
*                                                                               
         LHI   R5,DBLOCK2-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                        SET FIXED VALUES INTO          
         XC    DBLOCK,DBLOCK                     DBLOCK                         
         MVC   DBFILE,=C'TP '                    FILE,                          
         L     R0,AIO1                                                          
         ST    R0,DBAREC                         A(I/O AREA),                   
         MVI   DBFUNCT,DBGETDEM                  FUNCTION,                      
         MVC   DBCOMFCS,ACOMFACS                 A(COMFACS),                    
         MVC   DBSELSRC,DBSRC                    SOURCE,                        
         XC    DBSELBK,DBSELBK                   LATEST BOOK                    
         MVC   DBSELMED,DBMED                    MEDIA                          
         MVC   DBSELAGY,AGYALPH                  AGENCY CODE,                   
         MVI   DBSELDAY,X'40'                    DAY,                           
         MVC   DBSELTIM,=AL2(0800,0815)          AND TIMES,                     
                                                                                
         MVC   PREVSTTN,STACALL                                                 
         MVC   DBSELSTA,STACALL                  STATION CALL LETTERS,          
         MVC   DBSELMK,KMARKET                   SPILL MARKET,                  
                                                                                
         MVC   DBSELBK,BKS                       BOOK,                          
         MVC   DBBTYPE,BKS+2                                                    
                                                                                
GAF01    CLI   XSPLLFLG,C'Y'                                                    
         BE    *+10                                                             
         XC    DBSELBK,DBSELBK                                                  
                                                                                
GAF02    DS    0H                                                               
         XC    DBACTUAL,DBACTUAL                                                
         MVC   AFFILS,SPACES                     INITIALIZE TO BLANKS           
                                                                                
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,GAFHK,0                                     
         OI    DBMODE,DBMDSEQ                                                   
                                                                                
GAF04    CLC   =C'KFRE',STACALL             FRESNO CHANGES                      
         BNE   GAF06                                                            
         CLC   =X'01D2',MARKET                                                  
         CLC   KBOOK,=X'6502'                                                   
         BL    GAF06                                                            
         MVC   AFFILS(1),=C'W'                                                  
GAF06    DS    0H                                                               
GAFX     J     EXIT                                                             
                                                                                
*******************************************************                         
GAFHK    NTR1                                                                   
         MVC   WORK,SPACES                                                      
**       GOTOR VDEFINE,DMCB,=C'AFFL',DBLOCK,WORK,RR=SRVRRELO                    
         GOTO1 VDEFINE,DMCB,=C'AFFL',DBLOCK,WORK                                
         MVC   AFFILS,WORK                                                      
GAFHKX   J     EXIT                                                             
         DROP  R5                                                               
                                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*             TRANSLATE ALPHA MARKET ROUTINE                          *         
*                                                                     *         
***********************************************************************         
* Calls DEMAND to translate an alpha market into a numeric market               
* At entry, DUB(3)   = alpha market padded w/ spaces                            
* At exit,  DUB+3(2) = numeric market                                           
                                                                                
TRAMKT   NTR1                                                                   
         LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                        R5=A(DBLOCK)                   
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,DBFIL                                                     
         MVC   DBAREC,AIO2                                                      
         MVI   DBFUNCT,DBCNVA2N                                                 
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBBTYPE,BKS+3                                                    
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELBK,BKS                                                      
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELALF,DUB                                                     
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
                                                                                
         MVC   DUB+3(2),DBSELRMK                 MARKET # RETURNED              
         CLI   DBERROR,0                         IN DBSELRMK                    
         BE    TRAX                              USE IT IF NO ERROR             
         XC    DUB+3(2),DUB+3                    ELSE, RETURN NULLS             
         DROP  R5                                                               
                                                                                
TRAX     DS    0H                                                               
         J     EXITY                                                            
         DROP  RB                                                               
         LTORG                                                                  
SPILLEQU EQU   X'80'                                                            
PUBEQU   EQU   X'40'                                                            
CABLEEQU EQU   X'20'                                                            
AFFILS   DS    CL5                                                              
PREVSTTN DS    CL(L'STACALL)                                                    
ASTATTAB DS    A                                                                
ASTATPTR DS    A                                                                
* TABLE OF FILE/DIRECTORY NAMES FOR I/O PROCESSOR                               
*SORTTAB  DS    CL5000                                                          
         SPACE 2                                                                
                                                                                
***********************************************************************         
*                                                                     *         
*   SPILL MARKET LIST FOR STATION                                     *         
*                                                                     *         
***********************************************************************         
                                                                                
SPILLIST NTR1  BASE=*                                                           
                                                                                
         MVI   DMCB,2                            SET LENGTH OF RECORD           
         MVC   DMCB+1(2),=X'0002'                                               
         MVC   ATSIOREC,AIO3                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
         MVC   DBFIL,=C'TP '                                                    
         MVI   DBSRC,C'N'                                                       
         MVI   DBMED,C'T'                                                       
*&&DO                                                                           
         LA    R3,KEY                            BUILD KEY FOR                  
         USING DMKEY,R3                          MARKET NAME RECORD             
         XC    DMKMAJOR,DMKMAJOR                 R3=A(KEY)                      
         MVI   DMCODE,DMCODEQU                                                  
         MVC   DMMEDIA,DBMED                                                    
         MVC   DMSRC,DBSRC                                                      
         MVI   IOFLAG,DIR+READ+DEM                                              
         GOTOR =A(IO),RR=SRVRRELO                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
* -----------------------------------------                                     
*  GET STATION AND TRANSLATE TO NUMERIC   |                                     
* -----------------------------------------                                     
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,ASTAT                                                       
         LA    R5,1                                                             
         TM    STAIND,LQ_TSINQ                                                  
         BNO   SPILL02                                                          
         LA    RE,LW_DATA1-LW_D(RE)                                             
         B     SPILL04                                                          
SPILL02  TM    STAIND,LQ_TLSTQ                                                  
         BO    *+6                                                              
         DC    H'0'                                                             
         ICM   R5,3,LW_NUMN-LW_D(RE)             GET NUMBER OF ENTRIES          
         LA    RE,LW_DATA2-LW_D(RE)              GET DATA IN WORK POOL          
                                                                                
SPILL04  LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                        R5=A(DBLOCK)                   
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIO2                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,DBFIL                                                     
         MVC   DBSELSTA,0(RE)                                                   
         OC    DBSELSTA,SPACES                                                  
         CLI   DBSELSTA+4,X'40'                                                 
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
                                                                                
         MVI   DBFUNCT,DBGETASM                                                 
SPILL06  DS    0H                                                               
                                                                                
SPILL08  EQU   *                                                                
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
                                                                                
SPILL10  DS    0H                                                               
         XC    MRKTNUM,MRKTNUM                                                  
                                                                                
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,SPILLHK,0                                   
         J     EXITY                                                            
         EJECT                                                                  
                                                                                
* -------------------------------------------------------------                 
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND       |                 
* -------------------------------------------------------------                 
                                                                                
SPILLHK  ST    RE,SAVERE                         SAVE RETURN ADDRESS            
         MVC   MKTNAME,SPACES                                                   
         MVC   MKTNAME(11),=C'**UNKNOWN**'                                      
         LA    R1,DBKEY                                                         
         USING BSKEY,R1                                                         
         MVC   MARKET,BSRMKT                     EXTRACT MARKET                 
         MVC   KMARKET,BSKMKT                                                   
         MVC   KBTYP,BSBTYP                                                     
         MVC   KBOOK,BSBOOK                                                     
         XC    KBOOK,=X'FFFF'                                                   
         OC    KMARKET,KMARKET                   SPILL MARKET ONLY              
         BZ    SPHOOKX                                                          
         CLI   DBMED,C'T'                        IF MEDIA IS USTV,              
         BNE   SPHOOK2                                                          
         CLI   KBTYP,X'05'                       & IT'S NSI 2A-6A DATA,         
         BE    SPHOOK2                           IGNORE THIS RECORD             
         CLI   KBTYP,C'C'                        AND BOOKTYPE IS CABLE,         
         BNE   SPHOOK2                                                          
SPHOOK2  OC    MARKET,MARKET                                                    
         BZ    SPHOOKX                                                          
*********************************************************************           
         LHI   R5,DBLOCK2-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBAREC,AIO1                                                      
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,DBFIL                                                     
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBSELMED,DBMED                                                   
                                                                                
         MVC   DBSELRMK,MARKET                                                  
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELSTA,STACALL    SET SPILL STATION                            
         LHI   R1,DBLOCK1-WORKD                                                 
         LA    R1,WORKD(R1)                                                     
         CLI   DBSELMED,C'N'                                                    
         BNE   *+10                                                             
         MVC   DBSELBK(2),DBSELBK-DBLOCKD(R1)                                   
         GOTO1 ASETUSID                                                         
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         MVC   MARKET,DBACTRMK                                                  
         LR    R1,R5               PT TO DBLOCK JUST GOT BACK                   
         LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         OI    DBMODE,DBMDSEQ      SET READ SEQUENCE BROKEN                     
         CLI   DBERROR-DBLOCKD(R1),0                                            
         BNE   SPHOOK8                                                          
         DROP  R5                                                               
DEMHOOK6 MVC   WORK,SPACES                                                      
         LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                                                       
         L     RE,AIO1                                                          
         ST    RE,DBAREC                                                        
         MVI   DBRECTYP,DBRECMK       FUDGE RECTYPE FOR DEFINE                  
         GOTO1 VDEFINE,DMCB,=C'MNAME',(R5),WORK                                 
**       MVC   MARKET,WORK+0                                                    
         XC    MKTNAME,MKTNAME                                                  
         MVC   MKTNAME,WORK+2                                                   
***      B     DEMHK6D                                                          
                                                                                
*********************************************************************           
*&&DO                                                                           
         MVC   DMMINOR,MARKET                                                   
         MVI   IOFLAG,FIL+READ+DEM                                              
         GOTOR =A(IO),RR=SRVRRELO                                               
         BNE   SPHOOK8                                                          
                                                                                
SPHOOK4  L     R1,AIO1                                                          
         LA    R1,DMFRSTEL-DMKEY(R1)                                            
         USING DMELEM,R1                         R1=A(FIRST ELEMENT)            
         SR    RE,RE                                                            
SPHOOK6  CLI   DMELEM,0                          TEST E-O-R                     
         BE    SPHOOK8                                                          
         CLI   DMELEM,DMECODEQ                   TEST MKT NAME ELEMENT          
         BE    *+14                                                             
         IC    RE,DMLEN                                                         
         AR    R1,RE                                                            
         B     SPHOOK6                                                          
                                                                                
         MVC   MKTNAME,SPACES                    EXTRACT MKT NAME FROM          
         IC    RE,DMLEN                          ELEMENT                        
         SH    RE,=H'5'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MKTNAME(0),DMMNAME                                               
**********************************************************************          
*&&                                                                             
SPHOOK8  DS    0H                                                               
         LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                                                       
         MVC   MARRALF,SPACES                                                   
         CLI   DBSELMED,C'N'                                                    
         BE    SPHOOK14                                                         
         SR    R0,R0                             SAVE MKT NUMBERS IN R0         
         ICM   R0,12,DBSELMK                     H.O.2.B= SPILL MARKET#         
         ICM   R0,3,DBSELRMK                     L.O.2.B= RTG SVCE MKT#         
         XC    DBSELMK,DBSELMK                                                  
         MVC   DBSELRMK,MARKET                                                  
         MVC   WORK,SPACES                                                      
                                                                                
SPHOOK10 DS    0H                                                               
         OC    MARKET,MARKET                                                    
         BZ    SPHOOK12                                                         
**       GOTOR VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK,RR=SRVRRELO                   
         GOTO1 VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK                               
         MVC   MARRALF,WORK                                                     
SPHOOK12 STCM  R0,12,DBSELMK                     RESTORE SPILL MARKET#          
         STCM  R0,3,DBSELRMK                     RESTORE RTG SVCE MKT#          
         DROP  R5                                                               
SPHOOK14 DS    0H                                                               
* CHECK TO SEE IF RECORD EXIST                                                  
         XC    WORK,WORK                         2 BYTE SEARCH KEY              
         MVC   WORK(2),KMARKET                                                  
         MVC   WORK2,WORK                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#READTSR,AREADTSR),DMCB                                         
         CLI   DMCB,C'Y'                                                        
         BE    SPHOOKX                                                          
         MVC   WORK,WORK2                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#WRITTSR,AWRITTSR),DMCB                                         
                                                                                
         LA    R4,MRKTVALS                                                      
         USING MRKTVALS,R4                                                      
         XC    ALFMKT,ALFMKT                                                    
         MVC   ALFMKT(L'MARRALF),MARRALF                                        
         EDIT  (B2,MARKET),(4,NUMMKT),ALIGN=LEFT                                
         MVC   HALF1,MRKTNUM                                                    
         MVC   MRKTNUM,=X'0001'                                                 
         CLC   MARRALF,SPACES                                                   
         BE    SPHOOK16                                                         
         OC    MARRALF,MARRALF                                                  
         BZ    SPHOOK16                                                         
         B     SPHOOK18                                                         
SPHOOK16 EDIT  (B2,MARKET),(4,ALFMKT),ALIGN=LEFT                                
                                                                                
                                                                                
SPHOOK18 L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         XC    MRKTVALS,MRKTVALS                                                
         MVC   MRKTNUM,HALF1                                                    
                                                                                
***      DROP  R4,R3                                                            
         DROP  R4                                                               
                                                                                
SPHOOKX  L     RE,SAVERE                         RETURN TO DEMAND               
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO EXECUTE I/O TO DEMO FILES.                               *         
*                                                                     *         
* NTRY - IOFLAG  CONTAINS COMMAND NUMBER, FILE/DIRECTORY NUMBER       *         
*                AND FILE TYPE (DIRECTORY/FILE).                      *         
*        KEY     CONTAINS KEY FOR READ                                *         
*        NDXDA   CONTAINS D/A (MAY BE ZERO FOR FILE I/O)              *         
*        AIOAREA CONTAINS ADDRESS OF OUTPUT RECORD                    *         
*                                                                     *         
* ALL COMMANDS (SEE CMNDTAB) ARE AVAILABLE FOR BOTH FILE AND          *         
* DIRECTORY. FOR FILE COMMANDS A DIRECTORY READ IS EXECUTED IF NDXDA  *         
* IS BINARY ZEROES. AS DMREAD FOR A DANDX FILE IS NOT SUPPORTED THIS  *         
* IS EMULATED. FOR DIRECTORY COMMANDS KEY IS ALWAYS SAVED IN KEYSAVE  *         
* BEFORE I/O IS EXECUTED, KEY CONTAINS ACTUAL RECORD AFTER I/O.       *         
*                                                                     *         
* EXIT - IOFLAG CONTAINS ERROR CONDITION (ZERO MEANS OK)              *         
*        USER'S IO AREA CONTAINS RECORD AFTER I/O                     *         
*        CC=EQUAL IF I/O SUCCESSFUL                                   *         
*        CC=LOW IF DISK ERROR                                         *         
*        CC=HIGH FOR END-OF-FILE & NOT FOUND                          *         
***********************************************************************         
         SPACE 1                                                                
IO       NMOD1 IOWORKX-IOWORKD,**IO**,CLEAR=YES                                 
         USING IOWORKD,RC                        RC=A(LOCAL W/S)                
         MVC   IOWORK1,IOFLAG                    SAVE COMMAND FLAG              
         MVC   IOWORK2,IOWORK1                                                  
         PACK  IOWORK3,IOWORK1                   INVERT COMMAND FLAG            
         NI    IOWORK2,X'07'                     IOWORK2=FIL/DIR NUMBER         
         ZIC   RE,IOWORK2                                                       
         LA    RF,L'FDTAB                                                       
         MR    RE,RE                                                            
         LA    RE,FDTAB-L'FDTAB(RF)                                             
IO2      MVC   IOFILE,0(RE)                      SET FILE/DIR NAMES             
         MVC   IODIR,7(RE)                                                      
         MVC   IOINDS,14(RE)                     SET FIL/DIR INDICATORS         
         NI    IOWORK3,X'03'                     IOWORK3=COMMAND NUMBER         
         ZIC   RE,IOWORK3                                                       
         SLL   RE,3                                                             
         LA    RE,CMNDTAB-8(RE)                                                 
         MVC   IOCMND,0(RE)                      SET COMMAND                    
         TM    IOWORK1,DIR                                                      
         BZ    IO6                                                              
*                                                DIRECTORY I/O CALL             
IO4      MVC   KEYSAVE,KEY                       SAVE KEY                       
         GOTO1 VDATAMGR,IOPARM,IOCMND,IODIR,KEY,AIO1                            
         L     RF,AIO1                           EXTRACT D/A FROM REC           
         SR    R0,R0                                                            
         ICM   R0,1,IODADSP                      UNLESS DISPLACEMENT            
         BZ    *+12                              IS ZERO                        
         AR    RF,R0                             RF=A(D/A)                      
         MVC   NDXDA,0(RF)                                                      
         MVC   IOFLAG,8(R1)                      RETURN DATAMGR ERROR           
         ZICM  RF,IODSDSP,(1)                    EXTRACT STATUS BYTE            
         BZ    *+14                                                             
         A     RF,AIO1                           RF-->STATUS BYTE               
         MVC   DSTATUS,0(RF)                                                    
         B     IOX                                                              
*                                                FILE CALL                      
IO6      OC    NDXDA,NDXDA                       TEST IF D/A PRESENT            
         BNZ   IO8                                                              
         MVC   IOFLAG,IOWORK2                                                   
         OI    IOFLAG,DIR+READ                   NO - DO DIRECTORY READ         
******   BAL   RE,IO                                                            
         BAS   RE,IO                                                            
         BNE   IOX                                                              
         OC    NDXDA,NDXDA                       TEST IF D/A PRESENT            
         BNZ   IO8                                                              
         MVI   IOFLAG,NOTFOUND                   NO - RETURN NOT FOUND          
         B     IOX                                                              
*                                                NON-DANDX FILE FUDGES          
IO8      CLI   IOFILTY,0                                                        
         BE    IO10                                                             
         CLI   IOFILTY,2                         TEST DIRECTORY ONLY            
         BNE   *+6                               (NO FILE)                      
         DC    H'0'                                                             
         CLC   IOCMND,CMNDREAD                   TEST DMREAD/FILE               
         BNE   *+10                                                             
         MVC   IOCMND,CMNDGETR                   YES-SET TO GETREC/FILE         
         B     IO12                                                             
                                                                                
IO10     L     RE,AIO1                           BUILD KEY IN USER,             
         ZIC   RF,IOKEYLN                        I/O AREA                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),KEY                       SET READ HIGH KEY              
*                                                IN I/O AREA                    
         DS    0H                  SUPPORT FOR DEMDIRN/DEMDIRO MERGE            
         CLI   IOWORK2,DEM                       APPLICABLE FOR,                
         BNE   IO10M                             DEMFIL ONLY                    
         TM    IOWORK1,FIL                       APPLICABLE FOR                 
         BZ    IO10M                             DEMFIL ONLY                    
         CLC   IOCMND,CMNDRSEQ                   NOT APPLICABLE FOR             
         BE    IO10M                             READ SEQUENTIAL                
         ZICM  RF,IOFSDSP,(1)                    GET DSPL OF FIL'S              
         BNZ   *+6                               STATUS BYTE                    
         DC    H'0'                                                             
         AR    RF,RE                                                            
         MVC   0(1,RF),DSTATUS                   AND MOVE STATUS BYTE           
IO10M    EQU   *                                 IN FOR READ                    
                                                                                
         CLC   IOCMND,CMNDREAD                   TEST IF READ COMMAND           
         BNE   *+10                                                             
         MVC   IOCMND,CMNDRDHI                   YES - SET COMMAND TO           
*                                                READ HIGH                      
*                                                FILE I/O CALL                  
IO12     GOTO1 VDATAMGR,IOPARM,IOCMND,IOFILE,NDXDA,AIO1,IOWORK                  
         MVC   IOFLAG,8(R1)                      RETURN DATAMGR ERROR           
         ZICM  RF,IOFSDSP,(1)                    EXTRACT STATUS BYTE            
         BZ    *+14                              FROM RECORD                    
         A     RF,AIO1                           RF-->STATUS BYTE               
         MVC   FSTATUS,0(RF)                                                    
                                                                                
         CLI   IOFILTY,0                         TEST IF A DANDX FILE           
         BNE   IOX                               NO - EXIT                      
         CLC   IOCMND,CMNDREAD                   TEST IF READ COMMAND           
         BNE   IOX                               NO - EXIT                      
         MVI   IOFLAG,NOTFOUND                                                  
         CLI   8(R1),0                                                          
         BNE   IOX                                                              
         L     RE,AIO1                           TEST RECORD FOUND              
         ZIC   RF,IOKEYLN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),KEY                                                      
         BNE   IOX                               NO-EXIT WITH NOT FOUND         
         MVI   IOFLAG,0                          RESET ERROR                    
*                                                SET CC & RETURN                
IOX      MVI   IOWORK1,1                                                        
         CLI   IOFLAG,0                                                         
         BE    IOXX                                                             
         MVI   IOWORK1,2                                                        
         TM    IOFLAG,EOF+NOTFOUND                                              
         BNZ   IOXX                                                             
         MVI   IOWORK1,0                                                        
IOXX     CLI   IOWORK1,1                                                        
         XIT1                                                                   
         DROP  RC                                                               
         EJECT                                                                  
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* TABLE OF FILE/DIRECTORY NAMES FOR I/O PROCESSOR                               
                                                                                
*                                 *======= DIR/FIL STATUS BYTES ======*         
DSTATUS  DS    XL1                 STATUS/CONTROL BYTE FROM DIRECTORY           
FSTATUS  DS    XL1                 STATUS/CONTROL BYTE FROM FILE                
FDTAB    DS    0XL19                                                            
         DC    C'DEMFIL DEMDIR ',AL1(0,20,19,22,18)                             
         DC    C'PAVFIL PAVDIR ',AL1(0,20,19,22,18)                             
         DC    C'SPTFIL SPTDIR ',AL1(1,13,14,15,13)                             
         DC    C'       STATION',AL1(2,00,00,00,17)                             
* TABLE OF VALID I/O COMMANDS FOR I/O PROCESSOR                                 
*                                                                               
CMNDTAB  DS    0CL8                                                             
CMNDRDHI DC    C'DMRDHI  '                                                      
CMNDRSEQ DC    C'DMRSEQ  '                                                      
CMNDREAD DC    C'DMREAD  '                                                      
CMNDGETR DC    C'GETREC  '                                                      
                                                                                
         SPACE 2                                                                
                                                                                
**********************************************************************          
*                                                                    *          
*        ROUTINE TO PROCESS MARKET LIST                              *          
*                                                                    *          
**********************************************************************          
                                                                                
MRKTLIST NTR1  BASE=*                                                           
         MVI   DMCB,2                                                           
         MVC   DMCB+1(2),=X'0002'                                               
         MVC   ATSIOREC,AIO2                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
         XC    BKS,BKS                                                          
         MVC   DBFIL,=C'TP '                                                    
         MVI   DBSRC,C'N'                                                       
         MVI   DBMED,C'T'                                                       
         LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                        R5=A(DBLOCK)                   
                                                                                
                                                                                
         XC    DBLOCK,DBLOCK                                                    
         L     R0,AIO1                                                          
         ST    R0,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,DBFIL                                                     
         MVI   DBFUNCT,DBGETAMB                                                 
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
                                                                                
         GOTOR (#SETUSID,ASETUSID)                                              
         LA    RE,MRKTVALS                                                      
         ST    RE,MRKTPTR                                                       
         XC    MRKTNUM,MRKTNUM                                                  
         GOTO1 VDEMAND,DMCB,DBLOCKD,MKTHOOK,0                                   
                                                                                
MRKTLX   J     EXITY                                                            
         EJECT                                                                  
* -------------------------------------------------------------                 
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND       |                 
* -------------------------------------------------------------                 
                                                                                
MKTHOOK  ST    RE,SAVERE                         SAVE RETURN ADDRESS            
*                                                SEARCH REC FOR NAME            
*                                                ELEMENT                        
         L     R1,DBAREC                                                        
         LA    R1,DMFRSTEL-DMKEY(R1)                                            
         USING DMELEM,R1                         R1=A(FIRST ELEMENT)            
         SR    RE,RE                                                            
         CLC   =X'6401',DBACTBK                                                 
         BH    MKTHOOKX                                                         
                                                                                
MKTHOOK2 CLI   DMELEM,0                          TEST E-O-R                     
         BE    MKTHOOKX                                                         
         CLI   DMELEM,DMECODEQ                   TEST MKT NAME ELEMENT          
         BE    *+14                                                             
         IC    RE,DMLEN                                                         
         AR    R1,RE                                                            
         B     MKTHOOK2                                                         
         MVC   MARNAME,SPACES                    EXTRACT MKT NAME FROM          
         MVC   MARKET,DMMNO                      ELEMENT                        
         IC    RE,DMLEN                                                         
         SH    RE,=H'5'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MARNAME(0),DMMNAME                                               
         DROP  R1                                                               
                                                                                
         MVC   WORK,SPACES                                                      
**       GOTOR VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK,RR=SRVRRELO                   
         GOTO1 VDEFINE,DMCB,=C'NAMKT',DBLOCK,WORK                               
         MVC   MARRALF,WORK                                                     
         LA    R4,MRKTVALS                                                      
         USING MRKTVALS,R4                                                      
         XC    ALFMKT,ALFMKT                                                    
         MVC   ALFMKT(L'MARRALF),MARRALF                                        
         MVC   WORK(L'MARKET),MARKET                                            
         ZICM  R1,WORK,2                                                        
         EDIT  (R1),(4,NUMMKT),ALIGN=LEFT                                       
         MVC   MKTNAME,MARNAME                                                  
         MVI   MRKTSEND,YESQ                                                    
         DROP  R4                                                               
                                                                                
         CLC   =C'**UNKNOWN**',MKTNAME                                          
         BE    MKTHOOKX                                                         
                                                                                
* CHECK TO SEE IF RECORD EXIST                                                  
         XC    WORK,WORK                         3 BYTE SEARCH KEY              
         MVC   WORK(2),MARKET                                                   
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         MVC   WORK2,WORK                                                       
         GOTOR (#READTSR,AREADTSR),DMCB                                         
         CLI   DMCB,C'Y'                                                        
         BE    MKTHOOKX                                                         
         MVC   WORK,WORK2                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#WRITTSR,AWRITTSR),DMCB                                         
                                                                                
         MVC  MRKTNUM,=X'0001'                                                  
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         MVC   MRKTNUM,DMCB+10                                                  
                                                                                
MKTHOOKX L     RE,SAVERE                         RETURN TO DEMAND               
         BR    RE                                                               
         DROP  RB                                                               
         LTORG                                                                  
**********************************************************************          
*                                                                    *          
*         PROCESS VALIDATION STATION LIST                            *          
*                                                                    *          
**********************************************************************          
VALSTATL NTR1  BASE=*                                                           
                                                                                
         SR    R4,R4                                                            
         ICM   R4,7,ASTAT                                                       
         LA    R5,1                                                             
         TM    STAIND,LQ_TSINQ                                                  
         BNO   VSTATL02                                                         
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     VSTATL04                                                         
VSTATL02 TM    STAIND,LQ_TLSTQ                                                  
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R5,3,LW_NUMN-LW_D(R4)             GET NUMBER OF ENTRIES          
         LA    R4,LW_DATA2-LW_D(R4)              GET DATA IN WORK POOL          
VSTATL04 LA    R3,VSTAVALS                                                      
         USING VSTAVALS,R3                                                      
VSTATL06 XC    VSTAVALS(VSTAVALL),VSTAVALS                                      
         MVC   VSTATION,0(R4)                                                   
         MVC   VSTAFLAG,11(R4)                                                  
         LA    R0,VSTATION                                                      
         MVI   VINVFLAG,C'N'                     DEFAULT NO INVENTORY           
         CLI   11(R4),C'N'                       IF INVALID STRING              
         BE    VSTATL08                          THEN NO INVENTORY              
         CLI   13(R4),C'Y'                       IF SPILL MKT THEN              
         BE    VSTATL08                          NO INVENTORY                   
         GOTOR =A(READINV),DMCB,(R0),RR=SRVRRELO READ INVENTORY                 
         CLI   DMCB+4,C'Y'                                                      
         JNE   *+8                                                              
         MVI   VINVFLAG,C'Y'                                                    
VSTATL08 AHI   R4,14                                                            
         MVC   VSTANUM,=X'0001'                                                 
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
                                                                                
         BCT   R5,VSTATL06                                                      
         DROP  R3                                                               
         XC    REQVALS(REQVALSL),REQVALS                                        
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
                                                                                
*-------------------------------------------------------------                  
* PROCESS INVENTORY RECORDS                                  |                  
* INPUT: DMCB+0 HAS ADDRESS OF STATION                       |                  
* OUTPUT: DMCB+8 HAS I BYTE FLAG                             |                  
*-------------------------------------------------------------                  
READINV  NTR1  BASE=*                                                           
                                                                                
         XC    WORK,WORK                                                        
         L     RE,0(R1)                          STATION FIELD                  
         LA    R3,WORK                                                          
         USING RINVKEY,R3                                                       
         MVI   RINVKTYP,RINVKTYQ                 REP RECORD                     
         MVC   RINVKREP,AGYALPH                                                 
         MVC   RINVKSTA,0(RE)                                                   
         OC    RINVKSTA(5),SPACES                                               
         CLI   RINVKSTA+3,C'+'                                                  
         BNE   RDINV034                                                         
         MVC   RINVKSTA+3(2),=C' 1'                                             
         B     RDINV036                                                         
RDINV034 CLI   RINVKSTA+4,C'+'                                                  
         BNE   RDINV035                                                         
         MVC   RINVKSTA+4(1),=C'1'                                              
         B     RDINV036                                                         
RDINV035 MVI   RINVKSTA+4,C'T'                                                  
         DROP  R3                                                               
                                                                                
RDINV036 L     R0,AIO1                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',WORK,KEY                     
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   KEY(RINVKINV-RINVKEY),WORK                                       
         BNE   RDINVNO                                                          
         CLI   KEY+(RINVKINV+3-RINVKEY),0          IF NOT OLD TYPE INV          
         BE    RDINV40                             FOUND-ITS GOOD               
         CLI   KEY+(RINVKRSR-RINVKEY),0            HAS TO BE HEADER REC         
         JNE   RDINV40                                                          
         J     RDINVYES                                                         
                                                                                
RDINV40  GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'REPDIR',WORK,KEY                     
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(RINVKINV-RINVKEY),WORK          IF NO MORE RECORDS           
         BNE   RDINVNO                             FOR THAT STATION             
         CLI   KEY+(RINVKINV+3-RINVKEY),0          IF NOT OLD TYPE INV          
         JE    RDINV40                             FOUND-ITS GOOD               
         CLI   KEY+(RINVKRSR-RINVKEY),0            HAS TO BE HEADER REC         
         JNE   RDINV40                                                          
         J     RDINVYES                                                         
                                                                                
RDINVNO  MVI  DMCB+4,C'N'                                                       
         J    READINVX                                                          
RDINVYES MVI  DMCB+4,C'Y'                                                       
                                                                                
READINVX J    EXIT                                                              
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
*-------------------------------------------------------------                  
* CLEAR OUTPUT AREAS                                         |                  
*-------------------------------------------------------------                  
CLRAREA  NTR1  BASE=*                                                           
         LA    R0,OUTVALS                        CLEAR DOWN SAVE AREA           
         LHI   R1,OUTVALSL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R0,OUTVALS2                       CLEAR DOWN SAVE AREA           
         LHI   R1,OUTVAL2L                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
**********************************************************************          
*                                                                    *          
* PROCESS DAYPART RECORDS                                            *          
*                                                                    *          
**********************************************************************          
READDYPT NTR1  BASE=*                                                           
* first get rep parent                                                          
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING RREPKEY,R3                                                       
         MVI   RSETKTYP,X'01'                    REP RECORD                     
         MVC   RSETKREP,AGYALPH                                                 
         DROP  R3                                                               
         L     R0,AIO1                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',WORK,WORK                    
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',WORK+28,AIO1,      +        
               MYDMWORK                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'01'                                                     
         L     R6,AIO1                                                          
         LA    R0,RSETELEM-RSETREC                                              
         STH   R0,DATADISP                                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RREPELEM,R6                                                      
         MVC   CPARREP,RREPPAR       SAVE PARENT REP CODE                       
         DROP  R6                                                               
                                                                                
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING RSETKEY,R3                                                       
         MVI   RSETKTYP,X'38'                    REP RECORD                     
**       MVC   RSETKREP,AGYALPH                                                 
         MVC   RSETKREP,CPARREP                                                 
         MVC   RSETKSET,=C'DP'                                                  
         MVC   RSETKID,=C'ALL '                                                 
         DROP  R3                                                               
                                                                                
         L     R0,AIO1                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',WORK,WORK                    
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',WORK+28,AIO1,      +        
               MYDMWORK                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,RSETMCDQ                                                  
         L     R6,AIO1                                                          
         LA    R0,RSETELEM-RSETREC                                              
         STH   R0,DATADISP                                                      
                                                                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    RE,DYPTVALS                                                      
         ST    RE,DYPTPTR                                                       
         XC    DYPRTNUM,DYPRTNUM                 INITIALIZE  N'DYPARTS          
         LR    R5,R6                                                            
         LA    R5,3(R5)                                                         
                                                                                
READ02   CLI   0(R5),0                                                          
         BE    READX                                                            
                                                                                
READ04   XC    WORK,WORK                         BUILD KEY                      
         LA    R3,WORK                                                          
         USING RRDPKEY,R3                                                       
         MVI   RRDPKTYP,X'3C'                    REP RECORD                     
         MVC   RRDPKREP,AGYALPH                                                 
         MVC   RRDPKDPT,0(R5)                                                   
         DROP  R3                                                               
                                                                                
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',WORK,WORK                    
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',WORK+28,AIO2,      +        
               MYDMWORK                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RE,DYPTPTR                                                       
         USING DYPTVALS,RE                                                      
         XC    DYPTVALS(DYPTVALL),DYPTVALS                                      
         L     R3,AIO2                                                          
         USING RRDPREC,R3                                                       
         MVC   DYPTCDES,0(R5)                                                   
         MVC   DYPTCDEL,RRDPSNAM                                                
         MVC   DYPTDISP,RRDPLNAM                                                
         MVI   DYPTSEND,YESQ                                                    
         DROP  RE                                                               
         DROP  R3                                                               
                                                                                
         AHI   R5,1                                                             
         L     RE,DYPTPTR                                                       
         LA    RE,DYPTVALL(RE)                                                  
         ST    RE,DYPTPTR                                                       
         ZICM  R1,DYPRTNUM,(3)                                                  
         AHI   R1,1                                                             
         STCM  R1,3,DYPRTNUM                                                    
                                                                                
         J     READ02                                                           
                                                                                
READX    L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR =A(CLRAREA),RR=SRVRRELO                                          
                                                                                
         J     EXITY                                                            
                                                                                
         GETEL R6,34,ELCODE                                                     
         LTORG                                                                  
MYDMWORK DS    12D                                                              
DYPTPTR  DS    A                                                                
CPARREP  DS    CL2                    PARENT REP FOR INV RECORDS                
         DROP  RB                                                               
                                                                                
**********************************************************************          
*                                                                    *          
*  VALIDATE BOOK FOR STATION DOWNLOAD                                *          
*                                                                    *          
**********************************************************************          
VBKLIST  NTR1  BASE=*                                                           
                                                                                
         MVC   DBFIL,=C'TP '                                                    
         MVI   DBMED,C'T'                                                       
         MVI   DBSRC,C'N'                                                       
         XC    NUMBKS,NUMBKS                                                    
                                                                                
         LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                        R5=A(DBLOCK)                   
         XC    DBLOCK,DBLOCK                                                    
         L     R0,AIO1                                                          
         ST    R0,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,DBFIL                                                     
                                                                                
* GET FILE, BOOK AND STATION INPUT LIST                                         
                                                                                
         SR    R4,R4                                                            
         ICM   R4,7,AFILE                                                       
         LA    R2,1                                                             
         TM    FILEIND,LQ_TSINQ                                                 
         BNO   VBOOKL02                                                         
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     VBOOKL04                                                         
VBOOKL02 TM    FILEIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R2,3,LW_NUMN-LW_D(R4)             GET NUMBER OF ENTRIES          
         LA    R4,LW_DATA2-LW_D(R4)              GET DATA IN WORK POOL          
         USING VBKSTAD,R4                                                       
VBOOKL04 MVC   INPFIL,VBKFILE                                                   
         MVC   TMPBOOK,VBKBOOK                   BOOKVAL 3 BYTE BOOK            
         MVC   TMPBKTYP,VBKBKTY                  BOOKTYPE                       
         MVC   TMPBKWK,VBKWEEK                   WEEK                           
         MVC   TMPINPBK(L'VBKINBK),VBKINBK       INPUTTED BOOK STRING           
         XC    TMPMULBK,TMPMULBK                                                
         MVC   TMPMULBK,VBKMBK                   MULTIBOOK                      
                                                                                
         LA    RE,VSTAVALS                                                      
         USING VSTAVALS,RE                                                      
         XC    VSTAVALS(VSTAVALL),VSTAVALS                                      
         MVC   VSTABOOK,TMPINPBK                 PASS BACK INPUT BOOK           
         DROP  RE                                                               
         OC    TMPINPBK,SPACES                                                  
         MVI   BKERRFLG,C'N'                                                    
         CLI   TMPBKWK,0                                                        
         BE    VBOOKL8F                                                         
         CLC   =C'TT',INPFIL                     ONLY TT AND PAV CAN            
         BE    VBOOKL8F                          HAVE WEEKS                     
         CLC   =C'PAV',INPFIL                                                   
         BE    VBOOKL8F                                                         
         MVI   VSTAFLAG,C'N'                                                    
         B     VBOOKL09                                                         
VBOOKL8F CLI   VBKVBFLG,C'N'                                                    
         JNE   VBOOKL10                                                         
VBOOKL09 MVI   VSTAFLAG,C'N'                     VALID BOOK EXPRESSION?         
         MVI   BKERRFLG,C'Y'                     SET BOOK ERRFLG                
                                                                                
VBOOKL10 XC    ALFMKTS,ALFMKTS                                                  
         XC    DBSELALF,DBSELALF                                                
         CLI   VSTAFLAG,C'N'                     IF INVALID FIELD FLAG          
         BE    VBOOKL11                          SET BY BOOK VALIDATION         
         CLI   VBKVSFLG,C'Y'                       DONT BOTHER, INVALID         
         BE    VBOOKL12                          STATION DONT BOTHER            
VBOOKL11 LA    R3,VSTAVALS                                                      
         USING VSTAVALS,R3                                                      
         XC    VSTAVALS(VSTAVALL),VSTAVALS                                      
         MVC   VSTABOOK,TMPINPBK                 PASS BACK INPUT BOOK           
         MVC   VSTATION,VBKINSTA                                                
         MVI   VSTAFLAG,C'N'                                                    
         DROP  R3                                                               
         CLI   OVSYS,8                           IF INVALID BOOK FOR            
         JNE   VBOOKL80                          REP/INV, READ IBKLIST          
         CLC   =C'INV',INPFIL                    ANYWAYS BECAUSE                
         BNE   VBOOKL80                          IT WILL CHECK IF IT            
*                                                MATCHES ALIAS INSTEAD          
VBOOKL12 XC    DUMSTATH,DUMSTATH                                                
         XC    DUMSTAT,DUMSTAT                                                  
         ZIC   RE,VBKSTATL                       LENGTH OF INPUT STRING         
         STC   RE,DUMSTATH+5                                                    
         MVI   DUMSTATH,L'DUMSTATH+L'DUMSTAT                                    
         ZIC   RE,VBKSTATL                       LENGTH OF INPUT STRING         
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMSTAT(0),VBKINSTA                                              
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMSTATH,(R0),C',=,/'                              
         L     R1,AIO1                                                          
         USING SCANBLKD,R1                                                      
         MVC   DBSELSTA,SC1STFLD                                                
         OC    DBSELSTA(5),=C'     '                                            
                                                                                
         CLC   =C'TT',INPFIL                                                    
         BE    *+10                                                             
         CLC   =C'T4',INPFIL                                                    
         BNE   VBOOKL14                                                         
         CLI   DBSELSTA+3,C'+'                                                  
         BNE   VBOOKL13                                                         
         MVI   DBSELSTA+3,C' '                                                  
         B     VBOOKL17                                                         
VBOOKL13 CLI   DBSELSTA+4,C'+'                                                  
         BNE   VBOOKL17                                                         
         MVI   DBSELSTA+4,C' '                                                  
         B     VBOOKL17                                                         
                                                                                
VBOOKL14 CLC   =C'PAV',INPFIL                                                   
         BNE   VBOOKL16                                                         
         CLI   DBSELSTA+3,C'+'                                                  
         BNE   VBOOKL15                                                         
         MVC   DBSELSTA+3(2),=C' 1'                                             
         B     VBOOKL18                                                         
VBOOKL15 CLI   DBSELSTA+4,C'+'                                                  
         BNE   VBOOKL17                                                         
         MVI   DBSELSTA+4,C'1'                                                  
         B     VBOOKL18                                                         
VBOOKL16 CLC   =C'INV',INPFIL                                                   
         BNE   VBOOKL17                                                         
         CLI   DBSELSTA+3,C'+'                 READ STAD RECORD OF              
         BNE   *+8                             PARENT ONLY                      
         MVI   DBSELSTA+3,C' '                                                  
         CLI   DBSELSTA+4,C'+'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C' '                                                  
                                                                                
VBOOKL17 CLI   DBSELSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
                                                                                
VBOOKL18 MVC   VBKINSTA,DUMSTAT                                                 
         MVC   ALFMKTS,SC2NDFLD                                                 
                                                                                
         LA    RF,SC2NDFLD                                                      
         ST    RF,DMCB                                                          
         ZIC   RF,SC2NDLEN                                                      
         OR    RF,RF                                                            
         BZ    VBOOKL21                                                         
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         XC    WORK,WORK                                                        
         GOTOR (#VALAMKT,AVALAMKT),DMCB                                         
         XC    SPILLMKT,SPILLMKT                                                
         OC    WORK(2),WORK                      BINARY MKT RETURNED?           
         BZ    VBOOKL21                          IF SO MKT IS NUMERIC           
         MVC   SPILLMKT,WORK                                                    
         XC    ALFMKTS,ALFMKTS                                                  
         B     VBOOKL22                                                         
                                                                                
         DROP  R1                                                               
VBOOKL21 OC    ALFMKTS,ALFMKTS                                                  
         BZ    VBOOKL22                                                         
         MVI   DBFUNCT,DBCNVA2N                  TRANSLATE ALPHA TO             
         OC    ALFMKTS,=X'404040'                NUMERIC MARKET                 
         MVC   DBSELALF,ALFMKTS                                                 
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                         ANY ERRORS?                    
         BNE   *+10                              NO, SET NUMERIC MKT            
         MVC   SPILLMKT,DBSELRMK                 AS SPILL                       
VBOOKL22 XC    DBSELRMK,DBSELRMK                 CLEAR FIELD IN CASE            
         MVI   DBERROR,0                         'TWAS FUDGED                   
                                                                                
VBOOKL24 MVI   DBFUNCT,DBGETMB                                                  
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELMK,SPILLMKT                                                 
                                                                                
VBOOKL30 LA    R3,VSTAVALS                       CALL DEMAND                    
         USING VSTAVALS,R3                                                      
         ST    R3,STATPTR                                                       
VBOOKL32 XC    VSTAVALS(VSTAVALL),VSTAVALS                                      
         MVC   VSTABOOK,TMPINPBK                 PASS BACK INPUT BOOK           
         MVC   VSTATION,VBKINSTA                                                
VBOOKL42 XC    VSTABOOK,VSTABOOK                                                
         GOTO1 VDATCON,DMCB,(3,TMPBOOK+1),(6,VSTABOOK)                          
         CLI   VSTABOOK+3,C'/'                   GET RID OF C'/'                
         BNE   *+10                                                             
         MVC   VSTABOOK+3(3),VSTABOOK+4          MOVE UP YEAR                   
         LA    RE,VSTABOOK+5                                                    
*&&DO                                                                           
         CLI   TMPBKTYP,X'81'               LOWERCASE                           
         BL    VBOOKL44                                                         
         CLI   TMPBKTYP,X'B9'                                                   
         BNH   *+12                                                             
         CLI   TMPBKTYP,C'A'                     BOOKTYPE                       
         BL    VBOOKL44                                                         
*&&                                                                             
         CLI   TMPBKTYP,0                                                       
         BE    VBOOKL44                                                         
         MVI   VSTABOOK+5,C'('                                                  
         MVC   VSTABOOK+6(1),TMPBKTYP            BOOKTYPE                       
*&&DO                                                                           
         CLI   TMPBKTYP,X'81'                                                   
         BL    *+16                                                             
         CLI   TMPBKTYP,X'B9'                                                   
         BH    *+8                                                              
         OI    VSTABOOK+6,X'40'           CAPITALLIZE BKTYPE                    
*&&                                                                             
*                                                                               
         LA    RE,TMPBKTYP                                                      
         ST    RE,DMCB                                                          
         LA    RE,VSTABOOK+6                                                    
         ST    RE,DMCB+8                                                        
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,,1,,12                                  
         L     RE,DMCB+8             A(OUTPUT AREA)                             
         CLI   0(RE),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,RE),=C'??'        JUST PUT OUT  ?? AS BOOKTYPE               
         CLI   1(RE),0               2 CHARACTER BOOKTYPE?                      
         BE    *+8                                                              
         CLI   1(RE),X'40'           2 CHARACTER BOOKTYPE?                      
         BE    *+16                                                             
         MVI   2(RE),C')'                                                       
         AHI   RE,3                                                             
         B     *+12                                                             
         MVI   1(RE),C')'                                                       
         AHI   RE,2                                                             
                                                                                
VBOOKL44 CLI   TMPBKWK,0                         WEEK                           
         JE    *+14                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),TMPBKWK                                                  
*  MULTIBOOK CODE                                                               
         OC    TMPMULBK,TMPMULBK                                                
         BZ    VBOOKL47                                                         
         MVI   0(RE),C'+'                                                       
         LR    R1,RE                                                            
         AHI   R1,1                                                             
         LA    R6,TMPMULBK                                                      
         LA    R0,3                                                             
VBOOKL45 ST    R1,FULL1                         SAVE R1                         
         GOTO1 VDATCON,DMCB,(3,0(R6)),(6,WORK)                                  
         L     R1,FULL1                                                         
         MVC   0(6,R1),WORK                                                     
         CLI   3(R1),C'/'                   GET RID OF C'/'                     
         BNE   *+10                                                             
         MVC   3(3,R1),4(R1)                MOVE UP YEAR                        
         AHI   R1,5                                                             
         LR    RE,R1                                                            
                                                                                
         CLI   TMPBKTYP,0                                                       
         BE    VBOOKL46                                                         
                                                                                
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP                BOOKTYPE                         
                                                                                
         LA    RF,TMPBKTYP                                                      
         ST    RF,DMCB                                                          
         LA    RF,1(RE)                                                         
         ST    RF,DMCB+8                                                        
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,,1,,12                                  
         L     RE,DMCB+8             A(OUTPUT AREA)                             
         CLI   0(RE),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,RE),=C'??'        JUST PUT OUT  ?? AS BOOKTYPE               
         CLI   1(RE),0               2 CHARACTER BOOKTYPE?                      
         BE    *+8                                                              
         CLI   1(RE),X'40'           2 CHARACTER BOOKTYPE?                      
         BE    *+16                                                             
         MVI   2(RE),C')'                                                       
         LA    R1,3(RE)                                                         
         B     *+12                                                             
         MVI   1(RE),C')'                                                       
         LA    R1,2(RE)                                                         
                                                                                
VBOOKL46 AHI   R6,2                                                             
         OC    0(2,R6),0(R6)                                                    
         BZ    VBOOKL47                                                         
         SHI   R0,1                                                             
         CHI   R0,0                                                             
         BE    VBOOKL47                                                         
         MVI   0(R1),C'+'                                                       
         AHI   R1,1                                                             
         J     VBOOKL45                                                         
                                                                                
VBOOKL47 CLI   TMPBOOK+1,X'FF'                   LET LATEST GO THROUGH          
         BNE   VBOOKL48                          AND                            
         MVC   VSTABOOK,TMPINPBK                 PASS BACK INPUT BOOK           
         DROP  R3                                                               
                                                                                
VBOOKL48 MVI   VSTAFLAG,C'N'                     DEFAULT NOT VALID              
         MVI   DBFUNCT,DBGETTLB                                                 
         MVC   DBSELBK,TMPBOOK+1                 PASS 2 BYTE BOOK               
         MVC   DBBTYPE,TMPBKTYP                                                 
         CLI   OVSYS,8                                                          
         JNE   VBOOKL49                                                         
         CLC   =C'INV',INPFIL                                                   
         BNE   VBOOKL49                                                         
         CLI   VBKSPLLF,C'Y'                     IF INV AND SPILL MKT           
         JE    VBOOKL80                          ITS INVALID                    
         MVI   DMCB,X'01'                                                       
         MVC   DMCB+1(1),VBKVBFLG                                               
         GOTOR =A(READIBKL),DMCB,RR=SRVRRELO                                    
         J     VBOOKL80                                                         
                                                                                
VBOOKL49 LA    R3,TMPMULBK                                                      
         MVI   BYTE1,1                                                          
VBOOKL50 GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                                                        
         BNE   VBOOKL80                                                         
*    PASS BACK ACTUAL BOOK AS ALIAS FOR LATEST BOOK                             
         OC    DBSELBK,DBSELBK                                                  
         BNZ   VBOOKL70                                                         
         MVI   DUMBKH,18                                                        
         XC    TMPACTBK,TMPACTBK                                                
         MVC   TMPACTBK+1(2),DBACTBK                                            
         GOTOR =V(UNBOOK),DMCB,(1,TMPACTBK),DUMBKH,0,(C'+',=CL6' '),   +        
               RR=SRVRRELO                                                      
         CLC   =C'INV',INPFIL                    TT,PAV LATEST                  
         BE    *+14                                                             
         MVC   VSTABOOK(6),=C'LATEST'                                           
         B     *+10                                                             
         MVC   VSTALIAS(L'DUMBK),DUMBK                                          
                                                                                
VBOOKL70 MVI   VSTAFLAG,C'Y'                                                    
         OC    0(2,R3),0(R3)                    NO MORE MULTIBOOK               
         BZ    VBOOKL80                                                         
         CLC   =C'LATEST',VSTABOOK              MULTIBK CANT                    
         JNE   VBOOKL74                         BE LATEST                       
         MVI   VSTAFLAG,C'N'                                                    
         J     VBOOKL80                                                         
                                                                                
VBOOKL74 ZIC   RE,BYTE1                         IF WE FINISHED ALL              
         AHI   RE,1                             MULTIBOOK                       
         STC   RE,BYTE1                                                         
         CLI   BYTE1,3                                                          
         BH    VBOOKL80                                                         
                                                                                
         MVI   VSTAFLAG,C'N'                    RESET FLAG                      
         MVC   DBSELBK,0(R3)                                                    
         AHI   R3,2                                                             
         J     VBOOKL50                                                         
                                                                                
VBOOKL80 MVC   VSTANUM,=X'0001'                                                 
         MVC   VSTINDX,VBKINDX                                                  
*  CHECK TO MAKE SURE MULTIBOOK IS ONLY FOR TT/T4                               
         OC    TMPMULBK,TMPMULBK                                                
         BZ    VBOOKL90                                                         
         CLI   INPFIL,C'T'                     ONLY TT AND T4                   
         BE    VBOOKL90                        HAS MULTIBOOK                    
         MVI   VSTAFLAG,C'N'                                                    
                                                                                
VBOOKL90 L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR CLRAREA                                                          
         XC    VSTAVALS(VSTAVALL),VSTAVALS                                      
                                                                                
         AHI   R4,VBKSTADQ                       NEXT TABLE ENTRY               
         XC    VSTAFLAG,VSTAFLAG                 CLEAR FLAG                     
         CLI   BKERRFLG,C'Y'                     IF BOOK WAS INVALID            
         BNE   *+8                               THEN INITIALIZE FLAG           
         MVI   VSTAFLAG,C'N'                     TO BE INVALID                  
         BCT   R2,VBOOKL04                       NEXT ENTRY                     
VBOOKLX  J     EXITY                                                            
         EJECT                                                                  
         LTORG                                                                  
DUMBKH   DS    CL8                                                              
DUMBK    DS    CL10                                                             
TMPACTBK DS    XL3                                                              
         DROP  RB                                                               
***********************************************************************         
*READ STAD RECORD AND GET IBKLIST                                     *         
*IF NO STAD OR IBKLIST SET UP,WE HAVE TO STILL ASSUME ITS A VALID BK  *         
*IF IBKLIST FOUND, BOOK HAS TO BE IN LIST TO BE VALID                 *         
*ENTRY DMCB+0 = 01   FOR VALIDATION OF BOOKLIST                       *         
*      DMCB+0 = 02   FOR BOOKLIST DOWNLOAD                            *         
*      DMCB+1 =  IF THE BOOK WAS PREVALIDATED CORRECTLY               *         
***********************************************************************         
                                                                                
READIBKL NTR1  BASE=*                                                           
         XC    TMP2CHBT,TMP2CHBT                                                
         CLI   DMCB,X'02'                        IF READING FULL LIST           
         JE    READIB16                                                         
         MVC   BYTE,DMCB+1                                                      
                                                                                
         XC    VSTABOOK,VSTABOOK                 PASS BACK INPUT BK AS          
         MVC   VSTABOOK,TMPINPBK                 DEFAULT UNLESS MATCHES         
                                                                                
*  IF INPUT WAS A VALID REAL BOOK PASS IT BACK AS EXPANDED BY DEFAULT           
         CLI   BKERRFLG,C'N'                     IF VALID REAL BOOK             
         BNE   READIB02                                                         
                                                                                
         XC    VSTABOOK,VSTABOOK                                                
                                                                                
         MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,TMPBOOK),MYBKH,0,(C'+',=CL6' '),     +        
               RR=SRVRRELO                                                      
         MVC   VSTABOOK(L'MYBK),MYBK                                            
                                                                                
         CLI   TMPBKTYP,0                                                       
         BE    READIB02                                                         
         LA    RE,VSTABOOK+5                                                    
READIB1A CLI   0(RE),C'A'                                                       
         BNL   READIB1B                                                         
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP            BOOKTYPE                             
                                                                                
                                                                                
*TRANSLATE BOOKTYPE TO 2 BYTE FORMAT                                            
         LA    R2,TMPBKTYP                                                      
         AHI   RE,1                                                             
         ST    RE,DMCB+8                                                        
                                                                                
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,(R2),1,,12                              
         L     RE,DMCB+8             A(OUTPUT AREA)                             
         CLI   0(RE),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,RE),=C'??'        JUST PUT OUT  ?? AS BOOKTYPE               
         MVC   TMP2CHBT,0(RE)        STORE 2 CHAR BOOKTYPE                      
                                                                                
         CLI   1(RE),0               2 CHARACTER BOOKTYPE?                      
         BE    *+8                                                              
         CLI   1(RE),X'40'           2 CHARACTER BOOKTYPE?                      
         BE    *+12                                                             
         MVI   2(RE),C')'                                                       
         B     *+8                                                              
         MVI   1(RE),C')'                                                       
         B     READIB02                                                         
READIB1B AHI   RE,1                                                             
         B     READIB1A                                                         
                                                                                
READIB02 XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RSTAKEY,R3                                                       
         MVI   RSTAKTYP,X'02'                    REP RECORD                     
         MVC   RSTAKREP,AGYALPH                                                 
         MVC   RSTAKSTA,DBSELSTA                                                
         CLI   RSTAKSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
         DROP  R3                                                               
                                                                                
         L     R0,AIO2                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,WORK                     
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'RSTAKEY),WORK               NO STAD RECORD ,VALID          
         JE    READIB03                                                         
         CLI   BKERRFLG,C'Y'                     REAL BOOK?                     
         JNE   READIB14                                                         
         MVC   VSTABOOK,TMPINPBK                 PASS BACK INPUT ONLY           
         J     EXITY                             AS INVALID                     
                                                                                
READIB03 GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',WORK+28,AIO2,      +        
               MYDMWRK2                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'48'                                                     
         L     R6,AIO2                                                          
                                                                                
         BAS   RE,GETEL2                                                        
         BE    READIB04                                                         
* NO BOOKS SETUP FOR STATION                                                    
         CLI   BKERRFLG,C'Y'                     IF NO IBK RECORD               
         JE    EXITY                             BOOK MUST BE VALID             
         J     READIB14                          IF BOOK IS VALID BOOK          
         USING RSTABLEL,R6                       WE HAVE TO ASSUME              
READIB04 MVC   STABLTG,RSTABLTG                  ITS VALID                      
         DROP  R6                                                               
                                                                                
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RIBLKEY,R3                                                       
         MVI   RIBLKTYP,X'15'                    REP RECORD                     
         MVI   RIBLKSTY,X'03'                                                   
         MVC   RIBLKREP,AGYALPH                                                 
         MVC   RIBLKTAG,STABLTG                                                 
*******  MVC   RIBLKBTY,TMPBKTYP                                                
         MVC   RIBLKBTY(2),TMP2CHBT                                             
         CLI   RIBLKBTY+1,X'40'                IBKLIST 2 BOOKTYPE               
         BNE   *+8                             CHAR IS NULL NOT SPACE           
         MVI   RIBLKBTY+1,0                                                     
                                                                                
         DROP  R3                                                               
                                                                                
         L     R0,AIO2                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,WORK                     
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    RE,RIBLKBTY+1-RIBLKEY       1 CHARACTER BOOKTYPE LEN             
         CLI   TMP2CHBT+1,0                                                     
         BE    *+8                                                              
         LA    RE,RIBLKBTY+2-RIBLKEY       2 CHARACTER BOOKTYPE LEN             
                                                                                
****     CLC   KEY(RIBLKBTY+1-RIBLKEY),WORK                                     
****     JNE   READIB14                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),WORK                                                      
         JNE   READIB14                                                         
                                                                                
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',WORK+28,AIO2,      +        
               MYDMWRK2                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO2                                                          
         XC    VSTABOOK,VSTABOOK                                                
         MVC   VSTABOOK,TMPINPBK                 DEFAULT UNLESS MATCHES         
                                                                                
         USING RIBLBOOK,R6                                                      
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         B     READIB08                                                         
                                                                                
READIB06 BAS   RE,NEXTEL2                                                       
         BNE   READIBX                                                          
                                                                                
READIB08 CLC   TMPINPBK(L'RIBLBKOV),RIBLBKOV    IF ALIAS MATCHED FINE           
         BE    READIB12                         ELSE CHECK BOOK TO              
READIB10 CLC   TMPBOOK,RIBLBKBK                                                 
         JNE   READIB06                                                         
READIB12 MVC   VSTALIAS,RIBLBKOV                                                
         XC    VSTABOOK,VSTABOOK                                                
         MVI   MYBKH,18                          CREATE FAKE FIELD              
         GOTOR =V(UNBOOK),DMCB,(1,RIBLBKBK),MYBKH,0,(C'+',=CL6' '),    +        
               RR=SRVRRELO                                                      
         MVC   VSTABOOK(L'MYBK),MYBK                                            
                                                                                
         CLI   TMPBKTYP,0                                                       
         BE    READIB14                                                         
         LA    RE,VSTABOOK+5                                                    
READI12B CLI   0(RE),C'A'                                                       
         BNL   READIB13                                                         
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),TMPBKTYP            BOOKTYPE                             
                                                                                
****     MVI   2(RE),C')'                                                       
*TRANSLATE BOOKTYPE TO 2 BYTE FORMAT                                            
         LA    R2,TMPBKTYP                                                      
         AHI   RE,1                                                             
         ST    RE,DMCB+8                                                        
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,(R2),1,,12                              
         L     RE,DMCB+8             A(OUTPUT AREA)                             
         CLI   0(RE),X'FF'           IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   0(2,RE),=C'??'        JUST PUT OUT  ?? AS BOOKTYPE               
                                                                                
         CLI   1(RE),0               2 CHARACTER BOOKTYPE?                      
         BE    *+8                                                              
         CLI   1(RE),X'40'           2 CHARACTER BOOKTYPE?                      
         BE    *+12                                                             
         MVI   2(RE),C')'                                                       
         B     *+8                                                              
         MVI   1(RE),C')'                                                       
                                                                                
         B     READIB14                                                         
READIB13 AHI   RE,1                                                             
         B     READI12B                                                         
READIB14 L     RE,STATPTR                                                       
         USING VSTAVALS,RE                                                      
         MVI   VSTAFLAG,C'Y'                                                    
         J      EXITY                                                           
         DROP  R6                                                               
                                                                                
READIB16 MVC   DUB(4),=C'SELW'                                                  
         XC    DMCB,DMCB                                                        
         LA    RF,DUB                                                           
         ST    RF,DMCB                                                          
         GOTOR (#GETPROF,AGETPROF),DMCB                                         
         TM    PROFSELW+1,X'20'                                                 
         BZ    READIBX                                                          
                                                                                
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RSTAKEY,R3                                                       
         MVI   RSTAKTYP,X'02'                    REP RECORD                     
         MVC   RSTAKREP,AGYALPH                                                 
         MVC   RSTAKSTA,DBSELSTA                                                
         CLI   RSTAKSTA+4,C'T'                                                  
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
         DROP  R3                                                               
                                                                                
         L     R0,AIO2                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,WORK                     
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(L'RSTAKEY),WORK               IF NO STAD REC                 
         JE    READIB18                          THEN EXIT                      
         J     EXITY                                                            
                                                                                
READIB18 GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',WORK+28,AIO2,      +        
               MYDMWRK2                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'48'                                                     
         L     R6,AIO2                                                          
                                                                                
         BAS   RE,GETEL2                                                        
         BE    READIB20                                                         
         J     EXITY                                                            
         USING RSTABLEL,R6                                                      
READIB20 MVC   NAMETAG,RSTABLTG                                                 
         XC    BKTYLIST,BKTYLIST                                                
         MVC   BKTYLIST+1(L'RSTABLTS),RSTABLTS                                  
         LA    R5,BKTYLIST                                                      
         J     READIB24                                                         
READIB22 MVI   NUMBTCHR,1                        ASSUME 1 CHR BT                
         CLI   0(R5),C','                        COMMA DELIMITER?               
         BE    READIB60                                                         
         CLI   0(R5),C'A'                        NO MORE BKTYPES                
         BL    READIBX                           THEN EXIT                      
         DROP  R6                                                               
                                                                                
READIB24 XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RIBLKEY,R3                                                       
         MVI   RIBLKTYP,X'15'                    REP RECORD                     
         MVI   RIBLKSTY,X'03'                                                   
         MVC   RIBLKREP,AGYALPH                                                 
         MVC   RIBLKTAG,NAMETAG                                                 
         XC    RIBLKBTY(2),RIBLKBTY                                             
* HAVE TO TRANSLATE FROM 2 CHARACTER BOOKTYPE FROM STAD LIST                    
* TO INTERNAL BOOKTYPE                                                          
         LA    R2,0(R5)                                                         
         CLI   0(R5),0                                                          
         JNE   *+18                                                             
         MVC   RIBLKBTY(1),0(R5)                                                
         MVI   NUMBTCHR,1                                                       
         J     READIB25                                                         
* IF 2ND CHAR IS ',' THEN 1 CHARACTER BKTYPE, ELSE 2 CHAR BKTYPE                
         LA    RE,2                                                             
         MVI   NUMBTCHR,2                                                       
         CLI   1(R5),C','            1 CHARACTER BOOKTYPE?                      
         JE    *+8                                                              
         CLI   1(R5),0               1 CHARACTER BOOKTYPE?                      
         JE    *+8                                                              
         CLI   1(R5),X'40'           1 CHARACTER BOOKTYPE?                      
         JNE   *+12                                                             
         LA    RE,1                                                             
         MVI   NUMBTCHR,1                                                       
                                                                                
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RIBLKBTY(0),0(R5)                                                
*****    DROP  R3                                                               
                                                                                
READIB25 L     R0,AIO2                                                          
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'REPDIR',KEY,WORK                     
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(RIBLKBTY+1-RIBLKEY),WORK                                     
         JNE   READIB60                                                         
                                                                                
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'REPFILE',WORK+28,AIO2,      +        
               MYDMWRK2                                                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO2                                                          
         USING RIBLBOOK,R6                                                      
         BAS   RE,GETEL2                                                        
                                                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         B     READIB28                                                         
                                                                                
READIB26 BAS   RE,NEXTEL2                                                       
         BNE   READIB60                                                         
                                                                                
READIB28 DS    0H                                                               
******   MVC   BOOKTYPE(1),0(R5)                                                
         MVC   BOOKTYPE(2),RIBLKBTY                                             
         XC    WORK,WORK                         3 BYTE SEARCH KEY              
                                                                                
*&&DO                                                                           
READIB30 MVC   WORK(1),BOOKTYPE                                                 
         MVC   WORK+2(3),RIBLBKBK                                               
*  REVERSE  ORDER                                                               
         XC    WORK+3(2),=X'FFFF'                                               
         MVC   WORK+5(L'RIBLBKOV),RIBLBKOV                                      
         MVC   WORK+1(1),RIBLBKBK                BOOK CODE                      
*&&                                                                             
* CHANGED FOR 2 CHARACTER BOOKTYPE                                              
READIB30 MVC   WORK(2),BOOKTYPE                                                 
         MVC   WORK+3(3),RIBLBKBK                                               
*  REVERSE  ORDER                                                               
         XC    WORK+4(2),=X'FFFF'                                               
         MVC   WORK+6(L'RIBLBKOV),RIBLBKOV                                      
         MVC   WORK+2(1),RIBLBKBK                BOOK CODE                      
                                                                                
* CHECK TO SEE IF RECORD EXIST                                                  
READIB34 MVC   WORK2,WORK                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#READTSR,AREADTSR),DMCB                                         
         CLI   DMCB,C'Y'                                                        
         BE    READIB40                                                         
         MVC   WORK,WORK2                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#WRITTSR,AWRITTSR),DMCB                                         
                                                                                
READIB40 J     READIB26                          NEXT BOOK                      
READIB60 AHI   R5,1                              DO NEXT BKTYPE                 
         CLI   NUMBTCHR,2                                                       
         BNE   *+8                               BUMP R5 AHEAD BY 1             
         AHI   R5,1                              IF 2 CHRACTER BKTYPE           
         J     READIB22                                                         
                                                                                
READIBX  J     EXITY                                                            
                                                                                
         GETEL2 R6,34,ELCODE                                                    
         DROP  R6,R3                                                            
***********************************************************************         
         LTORG                                                                  
MYBKH    DS    CL8                                                              
MYBK     DS    CL10                                                             
MYDMWRK2 DS    12D                                                              
STABLTG  DS    CL8                                                              
MYDATDSP DS    H                                                                
NAMETAG  DS    CL8                                                              
BKTYLIST DS    CL(L'RSTABLTS+1+1)   +1 BYTE FOR STANDARD AND +1 FOR EOL         
*                                                                               
NUMBTCHR DS    X                                                                
         DROP  RB                                                               
***********************************************************************         
*                                                                     *         
*          BOOK LIST DOWNLOAD                                         *         
*                                                                     *         
***********************************************************************         
BOOKLIST NTR1  BASE=*                                                           
****     MVI   DMCB,3                                                           
****     MVC   DMCB+1(2),=X'0003'                                               
         MVI   DMCB,4             2 CHARACTER BOOKTYPE EXPAND TO 4              
         MVC   DMCB+1(2),=X'0004'                                               
         MVC   ATSIOREC,AIO2                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
         LA    RE,BKFILTAB                                                      
         ST    RE,BKFILPTR                                                      
                                                                                
BOOKL01  MVC   DBFIL,=C'TP '                                                    
         MVI   DBMED,C'T'                                                       
         MVI   DBSRC,C'N'                                                       
         CLI   3(RE),X'08'        IF FILE IS REP ONLY                           
         BNE   *+12                                                             
         CLI   OVSYS,X'08'                                                      
         BNE   BOOKL50                                                          
         CLI   3(RE),X'02'        IF FILE IS SPOT ONLY                          
         BNE   *+12                                                             
         CLI   OVSYS,X'02'                                                      
         BNE   BOOKL50                                                          
         XC    BKTYPNUM,BKTYPNUM                                                
         SR    R4,R4                                                            
         ICM   R4,7,ASTAT                                                       
         LA    R5,1                                                             
         MVC   MYFILE,0(RE)                                                     
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         XC    MYFILE,MYFILE                                                    
         XC    BKTYPNUM,BKTYPNUM                                                
         TM    STAIND,LQ_TSINQ                                                  
         BNO   BOOKL02                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     BOOKL04                                                          
BOOKL02  TM     STAIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R5,3,LW_NUMN-LW_D(R4)             GET NUMBER OF ENTRIES          
         LA    R4,LW_DATA2-LW_D(R4)              GET DATA IN WORK POOL          
                                                                                
BOOKL04  ST     R5,NUMSTAT                                                      
         LHI   R5,DBLOCK1-WORKD                                                 
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5                        R5=A(DBLOCK)                   
         XC    DBLOCK,DBLOCK                                                    
         L     R0,AIO1                                                          
         ST    R0,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,DBFIL                                                     
                                                                                
         XC    BKTYVALS,BKTYVALS                                                
         XC    ALFMKTS,ALFMKTS                                                  
         XC    DBSELALF,DBSELALF                                                
                                                                                
BOOKL06  XC    DUMSTATH,DUMSTATH                                                
         XC    DUMSTAT,DUMSTAT                                                  
         ZIC   RE,12(R4)                         LENGTH OF INPUT STRING         
         STC   RE,DUMSTATH+5                                                    
         MVI   DUMSTATH,L'DUMSTATH+L'DUMSTAT                                    
         ZIC   RE,12(R4)                         LENGTH OF INPUT STRING         
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DUMSTAT(0),0(R4)                                                 
         L     RE,AIO1                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R0,AIO1                                                          
         GOTO1 VSCANNER,DMCB,DUMSTATH,(R0),C',=,/'                              
         L     R1,AIO1                                                          
         USING SCANBLKD,R1                                                      
         MVC   DBSELSTA,SC1STFLD                                                
         OC    DBSELSTA(5),=C'     '                                            
         MVC   0(10,R4),DUMSTAT                                                 
         MVC   ALFMKTS,SC2NDFLD                                                 
                                                                                
         L     RE,BKFILPTR                                                      
*                                                 PARENTS PLUS                  
                                                                                
BOOKL07  CLC   =C'TT',0(RE)                                                     
         BE    *+10                                                             
         CLC   =C'T4',0(RE)                                                     
         BNE   BOOKL08                                                          
         CLI   DBSELSTA+3,C'+'                                                  
         BNE   BOOKL7A                                                          
         MVI   DBSELSTA+3,C' '                                                  
         B     BOOKL11                                                          
BOOKL7A  CLI   DBSELSTA+4,C'+'                                                  
         BNE   BOOKL11                                                          
         MVI   DBSELSTA+4,C' '                                                  
         B     BOOKL11                                                          
BOOKL08  CLC   =C'PAV',0(RE)                                                    
         BNE   BOOKL10                                                          
         CLI   DBSELSTA+3,C'+'                                                  
         BNE   BOOKL09                                                          
         MVC   DBSELSTA+3(2),=C' 1'                                             
         B     BOOKL12                                                          
BOOKL09  CLI   DBSELSTA+4,C'+'                                                  
         BNE   BOOKL11                                                          
         MVI   DBSELSTA+4,C'1'                                                  
         B     BOOKL12                                                          
BOOKL10  CLC   =C'INV',0(RE)                                                    
         BNE   BOOKL11                                                          
         CLI   DBSELSTA+3,C'+'                 READ STAD RECORD OF              
         BNE   *+8                             PARENT ONLY                      
         MVI   DBSELSTA+3,C' '                                                  
         CLI   DBSELSTA+4,C'+'                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C' '                                                  
                                                                                
BOOKL11  CLI   DBSELSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   DBSELSTA+4,C'T'                                                  
         CLC   =C'WTP',0(RE)                                                    
         BNE   *+8                                                              
         MVI   DBMED,C'W'                                                       
         CLC   =C'OTP',0(RE)                                                    
         BNE   BOOKL12                                                          
         MVI   DBMED,C'O'                                                       
* CHECK IF WE ARE REP CALLER.  ONLY PROPOSER HAS ACCESS TO OVERNIGHTS           
* FOR REP USER- COMPARAGRAPGH DOESNT SUPPORT OVERNIGHTS YET                     
* IF WE ARE REP- GRAB TODAYS DATE VIA DATCON CALL                               
         GOTO1 VDATCON,DMCB,(5,DUB1),(0,TODAYDAT)  YYMMDD                       
         GOTO1 VADDAY,DMCB,TODAYDAT,TODAYDAT,-1    YESTERDAY                    
         GOTO1 VADDAY,DMCB,TODAYDAT,LSTWKDAT,-6    LAST WEEK                    
         XC    DUB1,DUB1                                                        
*                                                                               
         MVI   BYTE2,1                                                          
         GOTO1 VNSIWEEK,DMCB,TODAYDAT,(BYTE2,VGETDAY),VADDAY,VDATCON            
         MVC   TODAYBK(1),4(R1)   YEAR                                          
         MVC   TODAYBK+1(1),0(R1) WEEK                                          
         MVI   BYTE2,1                                                          
         GOTO1 VNSIWEEK,DMCB,LSTWKDAT,(BYTE2,VGETDAY),VADDAY,VDATCON            
         MVC   LASTWKBK(1),4(R1)   YEAR                                         
         MVC   LASTWKBK+1(1),0(R1) WEEK                                         
         XC    DUB1,DUB1                                                        
         MVC   SVSELSTA,DBSELSTA                                                
* CALL GETDAY TO GRAB DAY OF THE WEEK FOR TODAYDAT -ALPHADAY=YESTERAY           
         GOTO1 VGETDAY,DMCB,TODAYDAT,ALPHADAY                                   
         GOTO1 VCALLOV,DMCB,0,X'D9000A03'                                       
         MVC   VDAYPAK,0(R1)                                                    
         GOTO1 VDAYPAK,DMCB,(3,ALPHADAY),BIYSTRDY,=X'17'                        
                                                                                
BOOKL12  CLC   =C'INV',0(RE)                                                    
         JNE   BOOKL17                                                          
                                                                                
****     MVI   DMCB,13                                                          
****     MVC   DMCB+1(2),=X'000D'                                               
         MVI   DMCB,14                                                          
         MVC   DMCB+1(2),=X'000E'      2 CHARCTER BOOKTYPE EXPAND BY 1          
         MVC   ATSIOREC,AIO2                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
         GOTOR =A(READIBKL),DMCB,X'02',RR=SRVRRELO                              
                                                                                
         LA    R5,1                                                             
BOOKL13  XC    DMCB,DMCB                                                        
         STCM  R5,3,DMCB                                                        
         GOTOR (#GETTSAR,AGETTSAR),DMCB                                         
         TM    DMCB+4,X'80'                                                     
         BO    BOOKL50                                                          
         L     R4,ATSIOREC                                                      
********************************************************                        
*&&DO                                                                           
         MVC   BOOKTYPE(1),0(R4)                                                
         MVI   MYBK2H,18                          CREATE FAKE FIELD             
                                                                                
         CLI   2(R4),0                           IS THIS AN ALIAS?              
         BE    BOOKL14                                                          
                                                                                
         MVC   BKTYDISP(L'RIBLBKOV),5(R4)                                       
BOOKL14  XC    3(2,R4),=X'FFFF'                                                 
         GOTOR =V(UNBOOK),DMCB,(1,2(R4)),MYBK2H,0,(C'+',=CL6' '),      +        
               RR=SRVRRELO                                                      
         MVC   BKTYBOOK(L'MYBK2),MYBK2                                          
                                                                                
BOOKL15  MVC   BKTYPNUM,=X'0001'                                                
         MVI   BKCAT,0                                                          
         TM    1(R4),X'20'                                                      
         BNO   *+8                                                              
         MVI   BKCAT,C'E'                                                       
         TM    1(R4),X'04'                                                      
         BNO   *+8                                                              
         MVI   BKCAT,C'P'                                                       
*&&                                                                             
********************************************************                        
*  2 CHARCTER BOOKTYPE                                                          
         MVC   BOOKTYPE(2),0(R4)                                                
         MVI   MYBK2H,18                          CREATE FAKE FIELD             
                                                                                
         CLI   3(R4),0                           IS THIS AN ALIAS?              
         BE    BOOKL14                                                          
                                                                                
         MVC   BKTYDISP(L'RIBLBKOV),6(R4)                                       
BOOKL14  XC    4(2,R4),=X'FFFF'                                                 
         GOTOR =V(UNBOOK),DMCB,(1,3(R4)),MYBK2H,0,(C'+',=CL6' '),      +        
               RR=SRVRRELO                                                      
         MVC   BKTYBOOK(L'MYBK2),MYBK2                                          
                                                                                
BOOKL15  MVC   BKTYPNUM,=X'0001'                                                
         MVI   BKCAT,0                                                          
         TM    2(R4),X'20'                                                      
         BNO   *+8                                                              
         MVI   BKCAT,C'E'                                                       
         TM    2(R4),X'04'                                                      
         BNO   *+8                                                              
         MVI   BKCAT,C'P'                                                       
*                                                                               
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR CLRAREA                                                          
         AHI   R5,1                                                             
         J     BOOKL13                                                          
                                                                                
BOOKL16  DS    0H                                                               
         J     BOOKL50                                                          
                                                                                
BOOKL17  LA    RF,SC2NDFLD                                                      
         ST    RF,DMCB                                                          
         ZIC   RF,SC2NDLEN                                                      
         OR    RF,RF                                                            
         BZ    BOOKL18                                                          
         ST    RF,DMCB+4                                                        
         LA    RF,WORK                                                          
         ST    RF,DMCB+8                                                        
         XC    WORK,WORK                                                        
         GOTOR (#VALAMKT,AVALAMKT),DMCB                                         
         XC    SPILLMKT,SPILLMKT                                                
         OC    WORK(2),WORK                      BINARY MKT RETURNED?           
         BZ    BOOKL18                           IF SO MKT IS NUMERIC           
         MVC   SPILLMKT,WORK                                                    
         XC    ALFMKTS,ALFMKTS                                                  
         B     BOOKL20                                                          
         DROP  R1                                                               
BOOKL18  OC    ALFMKTS,ALFMKTS                                                  
         BZ    BOOKL20                                                          
         MVI   DBFUNCT,DBCNVA2N                  TRANSLATE ALPHA TO             
         OC    ALFMKTS,=X'404040'                NUMERIC                        
         OC    ALFMKTS,=X'404040'                                               
         MVC   DBSELALF,ALFMKTS                                                 
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,0,0                                         
         CLI   DBERROR,0                         ANY ERRORS?                    
         BNE   *+10                                                             
         MVC   SPILLMKT,DBSELRMK                 NO, SET NUMERIC                
*                                                MARKET AS SPILL                
BOOKL20  XC    DBSELRMK,DBSELRMK                CLEAR FIELD IN CASE             
         MVI   DBERROR,0                         'TWAS FUDGED                   
                                                                                
         DS    0H                                                               
         MVI   DBFUNCT,DBGETMB                                                  
         MVC   DBSELMED,DBMED                                                   
         MVC   DBSELSRC,DBSRC                                                   
         MVC   DBSELAGY,AGYALPH                                                 
         MVC   DBSELMK,SPILLMKT                                                 
                                                                                
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,BOOKHK,0                                    
                                                                                
         AHI   R4,14                                                            
         L     R5,NUMSTAT                                                       
         BCT   R5,BOOKL04                                                       
                                                                                
         LA    R3,BKTYVALS                                                      
         USING BKTYVALS,R3                                                      
         LA    R5,1                                                             
BOOKL24  XC    DMCB,DMCB                                                        
         STCM  R5,3,DMCB                                                        
         GOTOR (#GETTSAR,AGETTSAR),DMCB                                         
         TM    DMCB+4,X'80'                                                     
         BO    BOOKL50                                                          
         L     R4,ATSIOREC                                                      
         L     RE,BKFILPTR                                                      
         CLC   =C'WTP',0(RE)                                                    
         BE    BOOKL25                                                          
         CLC   =C'OTP',0(RE)                                                    
         BE    BOOKL25                                                          
         XC    1(2,R4),=X'FFFF'                                                 
                                                                                
         GOTO1 VDATCON,DMCB,(3,1(R4)),(6,BKTYBOOK)                              
         CLI   BKTYBOOK+3,C'/'                   GET RID OF C'/'                
         JNE   *+10                                                             
         MVC   BKTYBOOK+3(3),BKTYBOOK+4          MOVE UP YEAR                   
         MVC   BOOKTYPE(L'SBBTYP),0(R4)                                         
*TRANSLATE BOOKTYPE TO 2 BYTE FORMAT                                            
         LA    R2,BOOKTYPE                                                      
         LA    R4,1(RE)                                                         
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,(R2),1,BOOKTYPE,12                      
         CLI   BOOKTYPE,X'FF'        IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   BOOKTYPE(2),=C'??'    SEND DOWN ?? AS BOOKTYPE                   
*                                                                               
         J     BOOKL52                                                          
                                                                                
BOOKL25  MVI   BYTE2,0                           SATURDAY                       
         CLI   DBMED,C'O'                        OVERNIGHTS                     
         BNE   *+8                                                              
         MVI   BYTE2,1                           MONDAY                         
         GOTO1 VNSIWEEK,DMCB,(C'D',1(R4)),(BYTE2,VGETDAY),VADDAY,      +        
               VDATCON                                                          
         ZICM  R1,DMCB+1,(7)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB1(6),0(R1)                                                    
*                                                                               
BOOKL26  GOTO1 VDATCON,DMCB,(0,DUB1),(5,BKTYBOOK)                               
* TRANSLATE BOOKTYPE                                                            
         MVC   BOOKTYPE(L'SBBTYP),0(R4)                                         
*TRANSLATE BOOKTYPE TO 2 BYTE FORMAT                                            
         LA    R2,BOOKTYPE                                                      
******   LA    R4,1(RE)                                                         
         MVC   SVBKTYPE,0(R4)                                                   
         GOTOR (#TRNSBKT,ATRNSBKT),DMCB,(R2),1,BOOKTYPE,12                      
         CLI   BOOKTYPE,X'FF'        IF BOOKTYPE IS NOT IN TABLE                
         BNE   *+10                  THEN SOMETHING IS REALLY AMISS             
         MVC   BOOKTYPE(2),=C'??'    SEND DOWN ?? AS BOOKTYPE                   
                                                                                
* IF REP USER FOR OVERNIGHTS FIGURE OUT THE ENDDATE                             
                                                                                
         CLI   OVSYS,8                           FOR REP                        
         JNE   BOOKL52                           AND OVERNIGHTS                 
         CLI   DBMED,C'O'                                                       
         BNE   BOOKL52                                                          
*                                                                               
         CLC   1(2,R4),TODAYBK       IF BEFORE CURRENT WEEK- PASS DOWN          
         JE    BOOKL46                                                          
*                                                                               
*                                                                               
         JL    *+6                                                              
         DC    H'0'                  IF HIGH- SOMETHING IS WRONG                
                                                                                
         CLI   BOOKTYPE+1,C'3'       LIVE+3?                                    
         BE    *+8                                                              
         CLI   BOOKTYPE,0            LIVE PLUS ??                               
         BNE   BOOKL40                                                          
* IF ITS LIVE PLUS WE HAVE TO CHECK IF WE ARE LOOKING AT LAST                   
* WEEK SINCE WE CAN ONLY BE DEALING WITH LAST WEEK AND NEVER CURRENT            
* WEEKS DATA FOR LIVE PLUS                                                      
* IF WE ARE DEALING WITH LAST WEEK THEN THIS WOULD BE THE LAST                  
* SET OF START AND END DATES PASSED DOWN TO PROPOSER                            
*                                                                               
         CLC   1(2,R4),LASTWKBK      LAST WEEK BOOK IS                          
         BL    BOOKL40               LAST WEEK OF LIVE PLUS DATA                
         BH    BOOKLX                                                           
* LAST WEEK OF LIVE PLUS DATA CHECK FOR ENDDATE                                 
* TO ACCURATELY GET THE ENDDATE FOR LIVE PLUS READ THE ENTIRE                   
* WEEK TO SEE IF WE GOT THE DATA IN YET STARTING FROM TUESDAY                   
                                                                                
*****    LA    R2,6    6 POSSIBLE DAYS STARTING FROM TUES-SUNDAY                
* LIVE+3 FOR OVERNIGHTS STARTS JAN2909                                          
         CLI   BOOKTYPE+1,C'3'       LIVE+3?                                    
         BNE   *+10                                                             
         CLC   1(2,R4),=X'6D05'                                                 
         BNE   *+10                                                             
         MVC   BKTYBOOK(8),=C'JAN29/09'                                         
         GOTO1 VDATVAL,DMCB,(0,BKTYBOOK),BKENDDAT  BKENDDAT=YYMMDD              
BOOKL30  GOTO1 VADDAY,DMCB,BKENDDAT,BKENDDAT,1                                  
         GOTO1 VGETDAY,DMCB,BKENDDAT,ALPHADY2                                   
         GOTO1 VDAYPAK,DMCB,(3,ALPHADY2),BISELDAY,=X'17'                        
         MVC   BYTE1,BISELDAY                                                   
***      MVC   SVBKTYPE,BOOKTYPE                                                
****     MVC   SVBKTYPE,SBBTYP                                                  
         MVC   LKUPBK,LASTWKBK                                                  
         GOTOR =A(CHKOVDAY),RR=SRVRRELO                                         
         CLI   FOUNDDAY,C'Y'                                                    
         JNE   BOOKL36                                                          
         CLI   BYTE1,X'01'          UNTIL DONE WITH SUNDAY OF WEEK              
         BNE   BOOKL30                                                          
*****    BCT   R2,BOOKL30                                                       
                                                                                
BOOKL36  DS    0H                                                               
         GOTO1 VADDAY,DMCB,BKENDDAT,BKENDDAT,-1                                 
         GOTO1 VDATCON,DMCB,(0,BKENDDAT),(5,BKENDDAT)                           
         B     BOOKL52                                                          
*                                                                               
*  IF LESS THAN TODAY'S DATE END DATE = 6 DAYS PASS THAT MONDAY                 
BOOKL40  GOTO1 VDATVAL,DMCB,(0,BKTYBOOK),BKENDDAT  BKENDDAT=YYMMDD              
         GOTO1 VADDAY,DMCB,BKENDDAT,BKENDDAT,6     SUNDAY ENDDATE               
         GOTO1 VDATCON,DMCB,(0,BKENDDAT),(5,BKENDDAT)                           
         J     BOOKL52                                                          
* IF CURRENT WEEK - CHECK TO SEE IF WE GOT YESTERDAYS DATA YET.                 
**BOOKL46  MVC   SVBKTYPE,BOOKTYPE                                              
BOOKL46  DS    0C                                                               
***      MVC   SVBKTYPE,SBBTYP                                                  
                                                                                
* IF ALPHA DAY= MON, DONT BOTHER READING THE FILE, WE SHOULD ONLY               
* HAVE MONDAY DATA.  SEND SAME END DATE.                                        
* WELL- IF TODAY IS TUESDAY, WE SHOULD ONLY HAVE MONDAY DATA.                   
* THE END DATE FOR MONDAY = MONDAY                                              
         CLC  ALPHADAY,=C'MON'                                                  
         JNE  *+14                                                              
         MVC  BKENDDAT,BKTYBOOK                                                 
         J    BOOKL52                                                           
                                                                                
         MVC   BYTE1,BIYSTRDY                                                   
         MVC   LKUPBK,TODAYBK                                                   
         GOTOR =A(CHKOVDAY),RR=SRVRRELO                                         
         GOTO1 VDATCON,DMCB,(0,TODAYDAT),(5,BKENDDAT)   DEFAULT                 
         CLI   FOUNDDAY,C'Y'                                                    
         JE    BOOKL52                                                          
* IF NOT FOUND                                                                  
         GOTO1 VADDAY,DMCB,TODAYDAT,TODAYDAT,-1   DAY BEFORE                    
         GOTO1 VDATCON,DMCB,(0,TODAYDAT),(5,BKENDDAT)                           
                                                                                
BOOKL52  MVC   BKTYPNUM,=X'0001'                                                
*&&DO                                                                           
* LIVE+3 FOR OVERNIGHTS STARTS JAN2909                                          
         CLI   BOOKTYPE+1,C'3'       LIVE+3?                                    
         BNE   *+10                                                             
         CLC   1(2,R4),=X'6D05'                                                 
         BNE   *+10                                                             
         MVC   BKTYBOOK(8),=C'JAN29/09'                                         
*&&                                                                             
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         GOTOR CLRAREA                                                          
BOOKL54  AHI   R5,1                                                             
         J     BOOKL24                                                          
                                                                                
                                                                                
BOOKL50  MVI   DMCB,3                            REINIT TSAR FOR EACH           
         MVC   DMCB+1(2),=X'0003'                NEW FILE READ                  
         MVC   ATSIOREC,AIO2                                                    
         GOTOR (#INITTSR,AINITTSR),DMCB                                         
                                                                                
BOOKL60  L     RE,BKFILPTR                                                      
         AHI   RE,L'BKFILTAB                                                    
         ST    RE,BKFILPTR                                                      
         OC    0(2,RE),0(RE)                                                    
         JZ    BOOKLX                                                           
         J     BOOKL01                                                          
                                                                                
BOOKLX   J     EXITY                                                            
         DROP  R3                                                               
*-----------------------------------------------------------------              
BOOKHK   ST    RE,SAVERE                         SAVE RETURN ADDRESS            
         L     R2,DBAREC                                                        
         USING SBKEY,R2                                                         
BKHK01   DS    0X                                                               
**       CLI   SBBTYP,C'I'                                                      
**       BE    BOOKHKX                                                          
         CLI   SBBTYP,C'Y'                                                      
         BE    BOOKHKX                                                          
         CLI   SBBTYP,C'G'                                                      
         BE    BOOKHKX                                                          
         TM    SBBOOK,X'80'                      BYPASS BAD STATION             
         BO    BOOKHKX                           RECORDS                        
         OC    SPILLMKT,SPILLMKT                                                
         BNZ   BKHK02                                                           
         OC    SBKMKT,SBKMKT                                                    
         BNZ   BOOKHKX                                                          
         B     BKHK04                                                           
BKHK02   CLC   SBKMKT,SPILLMKT                                                  
         BNE   BOOKHKX                                                          
BKHK04   LA    R3,BKTYVALS                                                      
         USING BKTYVALS,R3                                                      
         MVC   BOOKTYPE(L'SBBTYP),SBBTYP                                        
                                                                                
         CLI   DBMED,C'O'          OVERNIGHTS                                   
         BNE   *+14                ONLY AFTER DEC0604                           
         CLC   SBBOOK,=X'6832'                                                  
         BL    BOOKHKX                                                          
                                                                                
                                                                                
         CLC   SBBOOK,=X'5A07'                   IF BK IS PRIOR TO              
         BNL   *+8                               BOOKTYPE CUTOFF                
         MVI   BOOKTYPE,0                                                       
         CLI   SBBTYP,X'E0'                      IF XTRA SPILL                  
         BNE   *+8                                                              
         MVI   BOOKTYPE,0                                                       
         CLI   SBBTYP,X'05'                      IF X=05' NSI(2A-6A)            
         BNE   *+8                                                              
         MVI   BOOKTYPE,0                                                       
                                                                                
         L     RE,BKFILPTR                                                      
         CLI   BOOKTYPE,C'C'                                                    
         BNE   *+14                                                             
         CLC   =C'PAV',0(RE)                                                    
         BE    BOOKHKX                                                          
         CLI   BOOKTYPE,C'A'                                                    
         BNE   *+14                                                             
         CLC   =C'PAV',0(RE)                                                    
         BE    BOOKHKX                                                          
                                                                                
         XC    WORK,WORK                         3 BYTE SEARCH KEY              
         MVC   WORK(1),BOOKTYPE                                                 
         MVC   WORK+1(2),SBBOOK                                                 
         L     RE,BKFILPTR                                                      
         CLC   =C'OTP',0(RE)                                                    
         BE    *+10                                                             
         CLC   =C'WTP',0(RE)                                                    
         BE    *+10                                                             
         XC    WORK+1(2),=X'FFFF'                                               
                                                                                
* CHECK TO SEE IF RECORD EXIST                                                  
         MVC   WORK2,WORK                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#READTSR,AREADTSR),DMCB                                         
         CLI   DMCB,C'Y'                                                        
         BE    BKHK10                                                           
         MVC   WORK,WORK2                                                       
         LA    RE,WORK                                                          
         ST    RE,ATSIOREC                                                      
         GOTOR (#WRITTSR,AWRITTSR),DMCB                                         
         MVI   BKHKCNT,1                        WE WENT IN HOOK ALREADY         
                                                                                
BKHK10   DS    0H                                                               
         MVC   BKTYPNUM,DMCB+10                  RESTORE NUM OF ENTRIES         
BOOKHKX  L     RE,SAVERE                         RETURN TO DEMAND               
         BR    RE                                                               
                                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
BKFILTAB DC    0CL4                                                             
         DC    CL3'TT ',X'00'              SPOT AND REP                         
         DC    CL3'T4 ',X'00'              SPOT AND REP                         
         DC    CL3'PAV',X'08'              REP ONLY                             
         DC    CL3'INV',X'08'              REP ONLY                             
         DC    CL3'WTP',X'02'              SPOT ONLY                            
         DC    CL3'OTP',X'08'              SPOT AND REP                         
         DC    AL2(0)                                                           
                                                                                
MYDMWRK3 DS    12D                                                              
NUMSTAT  DS    F                                                                
BKFILPTR DS    A                                                                
MYBK2H   DS    CL8                                                              
MYBK2    DS    CL10                                                             
VDAYPAK  DS    A                                                                
         DROP  RB                                                               
         DROP  R5                                                               
         DROP  R3                                                               
***********************************************************************         
*                                                                     *         
*          VALIDATED DAYTIME LIST                                     *         
*                                                                     *         
***********************************************************************         
VDYTLIST NTR1  BASE=*                                                           
         SR    R4,R4                                                            
         ICM   R4,7,ADAYTIME                                                    
         LA    R5,1                                                             
         TM    DYTMIND,LQ_TSINQ                                                 
         BNO   VDYTML02                                                         
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     VDYTML04                                                         
VDYTML02 TM    DYTMIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R5,3,LW_NUMN-LW_D(R4)             GET NUMBER OF ENTRIES          
         LA    R4,LW_DATA2-LW_D(R4)              GET DATA IN WORK POOL          
VDYTML04 LA    R3,VDTMVALS                                                      
         USING VDTMVALS,R3                                                      
VDYTML06 XC    VDTMVALS(VDTMVALL),VDTMVALS                                      
         MVC   VDAYTIME,0(R4)                                                   
         MVI   VDTMFLAG,C'N'                                                    
         CLI   25(R4),0                          VALID DAYCODE FROM             
         BE    *+8                               VALIDATION ROUTINE?            
         MVI   VDTMFLAG,C'Y'                                                    
         OC    26(4,R4),26(R4)                   VALID TIMECODE FROM            
         BNE   *+8                               VALIDATION ROUTINE?            
         MVI   VDTMFLAG,C'N'                                                    
         MVC   VDTMNUM,=X'0001'                                                 
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         AHI   R4,30                                                            
         BCT   R5,VDYTML06                                                      
         J     EXITY                                                            
         LTORG                                                                  
         DROP  R3                                                               
         DROP  RB                                                               
***********************************************************************         
*                                                                     *         
*          VALIDATED DEMO LIST                                        *         
*                                                                     *         
***********************************************************************         
VDEMLIST NTR1  BASE=*                                                           
         SR    R4,R4                                                            
         ICM   R4,7,ADEMO                                                       
         LA    R5,1                                                             
                                                                                
         TM    DEMOIND,LQ_TSINQ                                                 
         BNO   VDEMOL02                                                         
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     VDEMOL04                                                         
VDEMOL02 TM    DEMOIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R5,3,LW_NUMN-LW_D(R4)             GET NUMBER OF ENTRIES          
         LA    R4,LW_DATA2-LW_D(R4)              GET DATA IN WORK POOL          
VDEMOL04 LA    R3,DEMOVALS                                                      
         USING DEMOVALS,R3                                                      
VDEMOL06 XC    DEMOVALS(DEMOVALL),DEMOVALS                                      
         MVC   DEMOS,4(R4)                       DEFAULT DEMO=INPUT             
         MVC   DEMOFLAG,3(R4)                    VALIDATION FLAG                
         CLI   3(R4),C'N'                        IF DEMO PASSED                 
         BE    VDEMOL08                          WAS INVALID                    
         MVC   DEMOS,4(R4)                                                      
                                                                                
VDEMOL08 MVC   DEMOMNUM,=X'0001'                                                
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         AHI   R4,24                                                            
         BCT   R5,VDEMOL06                                                      
                                                                                
         J     EXITY                                                            
         LTORG                                                                  
         DROP  R3                                                               
         DROP  RB                                                               
                                                                                
***********************************************************************         
* DOWNLOAD DEMO LIST                                                  *         
***********************************************************************         
                                                                                
DEMOLIST NTR1  BASE=*                                                           
         LA    R4,ADEMOSTB                                                      
         LA    R3,DEMLVALS                                                      
         USING DEMLVALS,R3                                                      
DEMOL02  OC    0(2,R4),0(R4)                                                    
         BE    DEMOLX                                                           
         L     R5,3(R4)                          A(DEMO TABLE)                  
         A     R5,SRVRRELO                                                      
DEMOL10  CLC   =X'0004',0(R5)                                                   
         BNE   DEMOL20                                                          
         MVC   DMCAT,2(R5)                       CATEGORY                       
         LA    R5,DMCATLEN(R5)                                                  
DEMOL20  MVC   DEMLDEMO(DEMOSLQ),0(R5)                                          
         MVC   DEMONAME(DEMOSNML),DMSDDISP(R5)                                  
         MVC   DEMOTYPE(DEMOTYPL),DMTPDISP(R5)                                  
         LA    R5,DEMOSTL(R5)                                                   
                                                                                
         MVC   DEMOMNUM,=X'0001'                                                
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
         XC    DMCAT,DMCAT                                                      
         XC    DEMLVALS(DEMLVALL),DEMLVALS                                      
         OC    0(2,R5),0(R5)                     END OF DEMO TABLE              
         BNZ   DEMOL10                           GET NEXT FILE                  
         LA    R4,L'ADEMOSTB(R4)                 NEXT FILE                      
         J     DEMOL02                                                          
DEMOLX   J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
ADEMOSTB DS    0CL7                                                             
         DC    CL3'TT ',AL4(DEMOSTB5)                                           
         DC    AL2(0)                                                           
       ++INCLUDE DELNKDEML                                                      
***********************************************************************         
* VALIDATE UPGRADE LIST                                               *         
***********************************************************************         
                                                                                
VALUPGD  NTR1  BASE=*                                                           
         ICM   R4,7,AUPGRD                                                      
         TM    UPGDIND,LQ_TSINQ                                                 
         BNO   VALUP02                                                          
         LA    R4,LW_DATA1-LW_D(R4)                                             
         J     VALUP04                                                          
VALUP02  TM    UPGDIND,LQ_TLSTQ                                                 
         JO    *+6                                                              
         DC    H'0'                                                             
         ICM   R5,3,LW_NUMN-LW_D(R4)             GET NUMBER OF ENTRIES          
         LA    R4,LW_DATA2-LW_D(R4)              GET DATA IN WORK POOL          
VALUP04  XC    UPGRDFLG,UPGRDFLG                                                
         MVC   UPGRDFLG,19(R4)                                                  
         L     R1,ALP                                                           
         L     RF,LP_APUTO-LP_D(R1)                                             
         GOTOR (RF)                                                             
VALUPGDX J     EXITY                                                            
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
* READ DEMOFILE TO CHECK WHETHER OVERNIGHTS DAY WAS LOADED            *         
***********************************************************************         
CHKOVDAY NTR1  BASE=*                                                           
         L     R5,=A(DBLOCK1-WORKD)                                             
         LA    R5,WORKD(R5)                                                     
         USING DBLOCKD,R5          R5=A(DBLOCK)                                 
                                                                                
         XC    DBLOCK,DBLOCK       SET FIXED VALUES INTO DBLOCK                 
         MVC   DBFILE,=C'TP '            FILE,                                  
         MVC   DBAREC,AIO1               A(I/O AREA),                           
         MVI   DBFUNCT,DBGETDEM          FUNCTION,                              
         MVC   DBCOMFCS,ACOMFACS         A(COMFACS),                            
         MVI   DBSELSRC,C'N'             SOURCE,                                
         MVC   DBSELBK,LKUPBK            BOOK,                                  
         MVI   DBSELMED,C'O'             OVERNIGHTS                             
         MVC   DBSELAGY,AGYALPH          AGENCY CODE,                           
         MVC   DBSELDAY,BYTE1            DAY,                                   
         MVC   DBSELTIM,=AL2(0800,0815)  AND TIMES,                             
         MVC   DBSELSTA,SVSELSTA                                                
         MVC   DBBTYPE,SVBKTYPE                                                 
         MVI   FOUNDDAY,C'N'             ASSUME NOT FOUND                       
         GOTOR (#SETUSID,ASETUSID)                                              
         GOTO1 VDEMAND,DMCB,DBLOCKD,CHKOVHK,0                                   
                                                                                
                                                                                
CHKOVX   J     EXITY                                                            
                                                                                
***********************************************************************         
* ROUTINE TO PROCESS VALUES RETURNED FROM SPDEMLK                               
* BE CAREFUL NOT TO CLOBBER DMCB, SINCE SPDEMLK'S CALLER DEPENDS ON IT          
***********************************************************************         
CHKOVHK  NTR1                                                                   
         CLC   SVBKTYPE,DBBTYPE           ONLY IF FOUND WHAT WE WANT            
         JNE   *+8                                                              
         MVI   FOUNDDAY,C'Y'              FOUND THE DAY                         
         J     EXITY                                                            
                                                                                
                                                                                
         LTORG                                                                  
         DROP  RB,R5                                                            
***********************************************************************         
* REQUEST DEFINITIONS                                                 *         
***********************************************************************         
                                                                                
REQUEST  DS    0X                                                               
***********************************************************************         
*  REQUEST FOR INITIAL DOWNLOAD                                       *         
***********************************************************************         
M#INIT   EQU   1                                                                
REQINIT  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQINITX+1-*)                                                
         DC    AL2(M#INIT)                                                      
         DC    AL1(0)                                                           
         DC    AL2(OUTINIT-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
* FUNCTION *                                                                    
                                                                                
CC#FUNCT EQU   1                                                                
CQ#FUNCT EQU   1                                                                
         DC    AL2(CQ#FUNCT)                                                    
         DC    CL5'FUNCT'                                                       
         DC    AL1(CC#FUNCT)                                                    
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FUNCIND-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(4,8)                                                         
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#FUNC)                                         
         DC    XL4'00'                                                          
                                                                                
REQINITX DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
*  REQUEST FOR STATIONS LIST                                          *         
***********************************************************************         
                                                                                
M#STATL  EQU   2                                                                
REQSTAT  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQSTATX+1-*)                                                
         DC    AL2(M#STATL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTSTAT-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
* MARKET *                                                                      
CC#MKT   EQU   1                                                                
CQ#MKT   EQU   1                                                                
         DC    AL2(CQ#MKT)                                                      
         DC    CL5'MRKET'                                                       
         DC    AL1(CC#MKT)                                                      
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALAMKT,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(AMKTIND-SAVED)                                               
         DC    AL1(7)         7 bytes output (2byte numeric+5 byte)             
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(0,5)                                                         
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#AMKT)                                         
         DC    XL4'00'                                                          
* BOOK *                                                                        
CC#BOOK  EQU   2                                                                
CQ#BOOK  EQU   2                                                                
         DC    AL2(CQ#BOOK)                                                     
         DC    CL5'BOOK '                                                       
         DC    AL1(CC#BOOK)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALBOOK,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(BOOKIND-SAVED)                                               
         DC    AL1(21)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(4,15)                                                        
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#1BBOK)                                        
         DC    XL4'00'                                                          
REQSTATX DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
*  REQUEST FOR MARKETS LIST                                           *         
***********************************************************************         
                                                                                
M#MRKTL  EQU   3                                                                
REQMRKTL DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQMRKTX+1-*)                                                
         DC    AL2(M#MRKTL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTMRKT-SVRDEF)                                              
         DC    XL4'00'                                                          
CC#MARKT EQU   1                                                                
CQ#MARKT EQU   1                                                                
         DC    AL2(CQ#MARKT)                                                    
         DC    CL5'MARKT'                                                       
         DC    AL1(CC#MARKT)                                                    
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FUNCIND-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(4,8)                                                         
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#FUNC)                                         
         DC    XL4'00'                                                          
                                                                                
REQMRKTX DC    AL1(LD_EOTQ)                                                     
                                                                                
*======================================================================         
*  REQUEST FOR VALIDATION STATION LIST                                *         
*======================================================================         
M#VALSTA EQU   4                                                                
RVALSTA  DS    0XL(LH_LNQ)                                                      
         DC    AL2(RVALSTAX+1-*)                                                
         DC    AL2(M#VALSTA)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTVSTAT-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
* STATION *                                                                     
CC#STAT  EQU   1                                                                
CQ#STAT  EQU   1                                                                
         DC    AL2(CQ#STAT)                                                     
         DC    CL5'STA  '                                                       
         DC    AL1(CC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(14)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(300,11)                                                      
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    XL4'00'                                                          
RVALSTAX DC    AL1(LD_EOTQ)                                                     
                                                                                
*======================================================================         
*  REQUEST FOR VALIDATION BOOK FOR STATIONS LIST                      *         
*======================================================================         
M#VALSBK EQU   5                                                                
RVALSBK  DS    0XL(LH_LNQ)                                                      
         DC    AL2(RVALSBKX+1-*)                                                
         DC    AL2(M#VALSBK)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTVSTBK-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
* FILE *                                                                        
EC#FILE  EQU   1                                                                
EQ#FILE  EQU   1                                                                
         DC    AL2(EQ#FILE)                                                     
         DC    CL5'FILE '                                                       
         DC    AL1(EC#FILE)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALFILE,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(FILEIND-SAVED)                                               
         DC    AL1(3)                                                           
         DC    AL1(LD_USERQ+LD_SCOLQ)                                           
         DC    AL1(100,3)                                                       
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#1BFIL)                                        
         DC    XL4'00'                                                          
* BOOK *                                                                        
EC#BOOK  EQU   2                                                                
EQ#BOOK  EQU   2                                                                
         DC    AL2(EQ#BOOK)                                                     
         DC    CL5'BOOK '                                                       
         DC    AL1(EC#BOOK)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#MULTBKS,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(BOOKIND-SAVED)                                               
         DC    AL1(52)         46 bytes +6 bytes for multibook                  
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(100,50)                                                      
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#1BBOK)                                        
         DC    XL4'00'                                                          
* STATION *                                                                     
EC#STAT  EQU   3                                                                
EQ#STAT  EQU   3                                                                
         DC    AL2(EQ#STAT)                                                     
         DC    CL5'STA  '                                                       
         DC    AL1(EC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(14)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(100,11)                                                      
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    XL4'00'                                                          
* PC INDEX                                                                      
EC#INDX  EQU   4                                                                
EQ#INDX  EQU   4                                                                
         DC    AL2(EQ#INDX)                                                     
         DC    CL5'INDEX'                                                       
         DC    AL1(EC#INDX)                                                     
         DC    AL1(LD_IAWPQ+LD_ILSTQ)                                           
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(INDXIND-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_UBINQ+LD_ECOLQ)                                           
         DC    XL2'00'                                                          
         DC    AL1(LD_IDSLQ,0)                                                  
         DC    AL1(SPSYSQ),AL2(SP#NUM)                                          
         DC    XL4'00'                                                          
RVALSBKX DC    AL1(LD_EOTQ)                                                     
                                                                                
*======================================================================         
*  REQUEST FOR DAYTIME VALIDATION                                     *         
*======================================================================         
M#VALDTM EQU   6                                                                
RVALDTM  DS    0XL(LH_LNQ)                                                      
         DC    AL2(RVALDTMX+1-*)                                                
         DC    AL2(M#VALDTM)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTDAYTM-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
* DAYTIME *                                                                     
EC#DYTM  EQU   1                                                                
EQ#DYTM  EQU   1                                                                
         DC    AL2(EQ#DYTM)                                                     
         DC    CL5'DYTM '                                                       
         DC    AL1(EC#DYTM)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ+LD_IDADQ)                         
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALDYTM,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DYTMIND-SAVED)                                               
         DC    AL1(30)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(40,300)                                                      
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#1BDYT)                                        
         DC    XL4'00'                                                          
RVALDTMX DC    AL1(LD_EOTQ)                                                     
                                                                                
*======================================================================         
*  REQUEST FOR DEMO VALIDATION                                        *         
*======================================================================         
M#VALDEM EQU   7                                                                
RVALDEM  DS    0XL(LH_LNQ)                                                      
         DC    AL2(RVALDEMX+1-*)                                                
         DC    AL2(M#VALDEM)                                                    
         DC    AL1(0)                                                           
         DC    AL2(OUTDEMOS-SVRDEF)                                             
         DC    XL4'00'                                                          
CC#DEMO  EQU   1                                                                
CQ#DEMO  EQU   1                                                                
         DC    AL2(CQ#DEMO)                                                     
         DC    CL5'DEMO '                                                       
         DC    AL1(CC#DEMO)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALDEMO,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(DEMOIND-SAVED)                                               
         DC    AL1(24)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(60,20)                                                       
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#DEMO)                                         
         DC    XL4'00'                                                          
RVALDEMX DC    AL1(LD_EOTQ)                                                     
                                                                                
                                                                                
***********************************************************************         
*  REQUEST FOR SPILL LIST                                             *         
***********************************************************************         
                                                                                
M#SPILL  EQU   8                                                                
REQSPILL DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQSPLLX+1-*)                                                
         DC    AL2(M#SPILL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTMRKT-SVRDEF)                                              
         DC    XL4'00'                                                          
                                                                                
* STATION *                                                                     
MC#STAT  EQU   1                                                                
MQ#STAT  EQU   1                                                                
         DC    AL2(MQ#STAT)                                                     
         DC    CL5'STA  '                                                       
         DC    AL1(MC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(14)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(300,11)                                                      
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    XL4'00'                                                          
                                                                                
REQSPLLX DC    AL1(LD_EOTQ)                                                     
                                                                                
*======================================================================         
*  REQUEST FOR BOOKS LIST                                             *         
*======================================================================         
M#BOOKL  EQU   9                                                                
RBOOKL   DS    0XL(LH_LNQ)                                                      
         DC    AL2(RBOOKLX+1-*)                                                 
         DC    AL2(M#BOOKL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTBOOKL-SVRDEF)                                             
         DC    XL4'00'                                                          
                                                                                
* STATION *                                                                     
BC#STAT  EQU   1                                                                
BQ#STAT  EQU   1                                                                
         DC    AL2(BQ#STAT)                                                     
         DC    CL5'STA  '                                                       
         DC    AL1(BC#STAT)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#VALSTA,0)                                                   
         DC    AL2(0)                                                           
         DC    AL2(STAIND-SAVED)                                                
         DC    AL1(14)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(300,11)                                                      
         DC    AL1(LD_IDSLQ)                                                    
         DC    XL1'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#STA)                                          
         DC    XL4'00'                                                          
RBOOKLX  DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
*  REQUEST FOR DEMOS LIST DOWNLOAD                                    *         
***********************************************************************         
                                                                                
M#DEMOL  EQU   10                                                               
REQDEMOL DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQDEMLX+1-*)                                                
         DC    AL2(M#DEMOL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTDEMOL-SVRDEF)                                             
         DC    XL4'00'                                                          
CC#DEM   EQU   1                                                                
CQ#DEM   EQU   1                                                                
         DC    AL2(CQ#DEM)                                                      
         DC    CL5'DEMOS'                                                       
         DC    AL1(CC#DEM)                                                      
         DC    AL1(LD_IAWPQ)                                                    
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED)                                                     
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(FUNCIND-SAVED)                                               
         DC    AL1(1)                                                           
         DC    AL1(LD_CHARQ)                                                    
         DC    AL1(4,8)                                                         
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#FUNC)                                         
         DC    XL4'00'                                                          
                                                                                
REQDEMLX DC    AL1(LD_EOTQ)                                                     
                                                                                
***********************************************************************         
*  REQUEST FOR UPGRADE FORMULA VALIDATION                             *         
***********************************************************************         
                                                                                
M#VALUP  EQU   13                                                               
REQVUPG  DS    0XL(LH_LNQ)                                                      
         DC    AL2(REQVUPGX+1-*)                                                
         DC    AL2(M#VALUP)                                                     
         DC    AL1(0)                                                           
         DC    AL2(OUTUPGD-SVRDEF)                                              
         DC    XL4'00'                                                          
CC#UPGD  EQU   1                                                                
CQ#UPGD  EQU   1                                                                
         DC    AL2(CQ#UPGD)                                                     
         DC    CL5'UPGD '                                                       
         DC    AL1(CC#UPGD)                                                     
         DC    AL1(LD_IRTNQ+LD_IAWPQ+LD_ILSTQ)                                  
         DC    AL1(0)                                                           
         DC    AL1(B#SAVED+LD_IUIR1)                                            
         DC    AL1(#PARSEUP,0)                                                  
         DC    AL2(0)                                                           
         DC    AL2(UPGDIND-SAVED)                                               
         DC    AL1(21)                                                          
         DC    AL1(LD_USERQ)                                                    
         DC    AL1(1,50)                                                        
         DC    XL2'00'                                                          
         DC    AL1(SPSYSQ),AL2(SP#OPT1)                                         
         DC    XL4'00'                                                          
                                                                                
REQVUPGX DC    AL1(LD_EOTQ)                                                     
                                                                                
REQUESTX DC    AL1(0)                                                           
***********************************************************************         
*======================================================================         
* OUTPUT MAP FOR INITIAL DOWNLOAD                                     |         
*======================================================================         
                                                                                
OUTINIT  DS    0X                                                               
R#INIT   EQU   1                   INITIAL DOWNLOAD                             
         DC    AL2(OUTINITX-*)                                                  
         DC    AL2(R#INIT)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
I#FILSRC EQU   1                   FILE SOURCE                                  
         DC    AL2(I#FLSRCX-*)                                                  
         DC    AL2(I#FILSRC),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#FILSRC)                                                    
         DC    CL5'FSRCE'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYFSRC-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
I#FLSRCX DS    0X                                                               
                                                                                
I#BKTYP  EQU   2                   BOOKTYPES                                    
         DC    AL2(I#BKTYPX-*)                                                  
         DC    AL2(I#BKTYP),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#BKTYP)                                                     
         DC    CL5'BKTYP'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYBKTYP-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
I#BKTYPX DS    0X                                                               
                                                                                
I#SETIME EQU   3                   START END TIMES                              
         DC    AL2(I#SETIMX-*)                                                  
         DC    AL2(I#SETIME),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#SETIME)                                                    
         DC    CL5'SETIM'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYSETIM-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
I#SETIMX DS    0X                                                               
                                                                                
I#DYTMAV EQU   4                   DAY TIME AVG                                 
         DC    AL2(I#DYTIMX-*)                                                  
         DC    AL2(I#DYTMAV),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#DYTMAV)                                                    
         DC    CL5'DYTIM'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYDYTIM-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
I#DYTIMX DS    0X                                                               
                                                                                
I#DAYPRT EQU   5                   DAYPARTS                                     
         DC    AL2(I#DYPRTX-*)                                                  
         DC    AL2(I#DAYPRT),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#DAYPRT)                                                    
         DC    CL5'DYPRT'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYDYPRT-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
I#DYPRTX DS    0X                                                               
                                                                                
                                                                                
I#DEMMOD EQU   17                  DEMO MODIFIER TABLE                          
         DC    AL2(I#DMMODX-*)                                                  
         DC    AL2(I#DEMMOD),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#DEMMOD)                                                    
         DC    CL5'DMMOD'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYDMMOD-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
I#DMMODX DS    0X                                                               
                                                                                
J#SPWKS  EQU   18                  SPECIFY WEEKS OPTION                         
         DC    AL2(J#SPWKSX-*)                                                  
         DC    AL2(J#SPWKS),C'SPWKS'                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(J#SPWKS)                                                     
         DC    CL5'SPWKS'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(OPTSPWKS-SAVED),AL1(LD_CHARQ,1)                              
         DC    XL4'00'                                                          
J#SPWKSX DS    0X                                                               
                                                                                
J#MBKS   EQU   19                  MUILTBOOKS OPTION                            
         DC    AL2(J#MBKSX-*)                                                   
         DC    AL2(J#MBKS),C'MBKS '                                             
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(J#MBKS)                                                      
         DC    CL5'MBKS '                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(OPTMBKS-SAVED),AL1(LD_CHARQ,1)                               
         DC    XL4'00'                                                          
J#MBKSX  DS    0X                                                               
                                                                                
                                                                                
I#BUFFS  EQU   20                  PASS BUFFER SIZE OF REPORT                   
         DC    AL2(I#BUFFSX-*)                                                  
         DC    AL2(I#BUFFS),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#BUFFS)                                                     
         DC    CL5'BUFFS'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYBUFFS-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
I#BUFFSX DS    0X                                                               
                                                                                
J#DEMON  EQU   21                  NUMBER OF DEMOS ALLOWED                      
         DC    AL2(J#DEMONX-*)                                                  
         DC    AL2(J#DEMON),C'DEMON'                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(J#DEMON)                                                     
         DC    CL5'DEMON'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(MAXNDEM-SAVED),AL1(LD_UBINQ,1)                               
         DC    XL4'00'                                                          
J#DEMONX DS    0X                                                               
I#PROFIL EQU   22                  profile settings                             
         DC    AL2(I#PROFLX-*)                                                  
         DC    AL2(I#PROFIL),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#PROFIL)                                                    
         DC    CL5'PROFL'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYPROF-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
I#PROFLX DS    0X                                                               
                                                                                
J#BKSTAN EQU   23                  NUMBER OF BOOKS FOR STATION                  
*                                  ALLOW PER TRANSACTION                        
         DC    AL2(J#BKSTAX-*)                                                  
         DC    AL2(J#BKSTAN),C'BKSTN'                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(J#BKSTAN)                                                    
         DC    CL5'BKSTN'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(MAXBKSTN-SAVED),AL1(LD_UBINQ,1)                              
         DC    XL4'00'                                                          
J#BKSTAX DS    0X                                                               
                                                                                
J#PRJFLG EQU   33                  PROJECTION ON/OFF                            
                                                                                
         DC    AL2(J#PRJFLX-*)                                                  
         DC    AL2(J#PRJFLG),C'PROJ '                                           
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(J#PRJFLG)                                                    
         DC    CL5'PROJ '                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(PROJFLAG-SAVED),AL1(LD_CHARQ,19)                             
         DC    XL4'00'                                                          
J#PRJFLX DS    0X                                                               
                                                                                
OUTINITX DS    0X                                                               
                                                                                
*======================================================================         
* OUTPUT MAP FOR STATIONS LIST                                        |         
*======================================================================         
                                                                                
OUTSTAT  DS    0X                                                               
R#STAT   EQU   2                   STATION LIST RECORD                          
         DC    AL2(OUTSTATX-*)                                                  
         DC    AL2(R#STAT)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
I#STATL  EQU   6                   STATION                                      
         DC    AL2(I#STATLX-*)                                                  
         DC    AL2(I#STATL),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#STATL)                                                     
         DC    CL5'STAT '                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYSTAT-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
I#STATLX DS    0X                                                               
                                                                                
OUTSTATX DS    0X                                                               
                                                                                
*======================================================================         
* OUTPUT MAP FOR MARKETS LIST                                         |         
*======================================================================         
                                                                                
OUTMRKT  DS    0X                                                               
R#MRKT   EQU   3                   MARKET LIST RECORD                           
         DC    AL2(OUTMRKTX-*)                                                  
         DC    AL2(R#MRKT)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
I#MRKTL  EQU   7                   MARKET LIST ARRY                             
         DC    AL2(I#MRKTLX-*)                                                  
         DC    AL2(I#MRKTL),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#MRKTL)                                                     
         DC    CL5'MRKET'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYMRKT-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
I#MRKTLX DS    0X                                                               
                                                                                
OUTMRKTX DS    0X                                                               
                                                                                
*======================================================================         
* OUTPUT MAP FOR VALIDATED STATIONS LIST                              |         
*======================================================================         
                                                                                
OUTVSTAT DS    0X                                                               
R#VSTAT  EQU   4                   STATION LIST RECORD                          
         DC    AL2(OUTVSTAX-*)                                                  
         DC    AL2(R#VSTAT)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
I#VSTATL EQU   8                   STATION                                      
         DC    AL2(I#VSTALX-*)                                                  
         DC    AL2(I#VSTATL),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(I#VSTATL)                                                    
         DC    CL5'STAT '                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYVSTA-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
I#VSTALX DS    0X                                                               
                                                                                
OUTVSTAX DS    0X                                                               
                                                                                
*======================================================================         
* OUTPUT MAP FOR VALIDATED BOOKS FOR STATION LIST                     |         
*======================================================================         
                                                                                
OUTVSTBK DS    0X                                                               
R#VSTBK  EQU   5                   BOOK FOR STATION VALID LIST                  
         DC    AL2(OUTVSTBX-*)                                                  
         DC    AL2(R#VSTBK)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
J#VSTATL EQU   9                   STATION                                      
         DC    AL2(J#VSTALX-*)                                                  
         DC    AL2(J#VSTATL),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(J#VSTATL)                                                    
         DC    CL5'STAT '                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYVSTA-SVRDEF),AL1(0,0)                                     
         DC    XL4'00'                                                          
J#VSTALX DS    0X                                                               
                                                                                
OUTVSTBX DS    0X                                                               
                                                                                
*======================================================================         
* OUTPUT MAP DAYTIME VALIDATION                                       |         
*======================================================================         
                                                                                
OUTDAYTM DS    0X                                                               
R#VDYTM  EQU   6                   BOOK FOR STATION VALID LIST                  
         DC    AL2(OUTDYTMX-*)                                                  
         DC    AL2(R#VDYTM)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
J#VDAYTM EQU   10                  daytime                                      
         DC    AL2(J#VDYTMX-*)                                                  
         DC    AL2(J#VDAYTM),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(J#VDAYTM)                                                    
         DC    CL5'DAYTM'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYVDYTM-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
J#VDYTMX DS    0X                                                               
                                                                                
OUTDYTMX DS    0X                                                               
*======================================================================         
* OUTPUT MAP DEMOS VALIDATION                                         |         
*======================================================================         
                                                                                
OUTDEMOS DS    0X                                                               
R#VDEMO  EQU   7                   BOOK FOR STATION VALID LIST                  
         DC    AL2(OUTDEMOX-*)                                                  
         DC    AL2(R#VDEMO)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
J#VDEMOS EQU   11                  DEMOS                                        
         DC    AL2(J#VDEMOX-*)                                                  
         DC    AL2(J#VDEMOS),C'ARRAY'                                           
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(J#VDEMOS)                                                    
         DC    CL5'DEMOS'                                                       
         DC    AL1(LO_IARRQ,0,0)                                                
         DC    AL2(ARYDEMOS-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
J#VDEMOX DS    0X                                                               
                                                                                
OUTDEMOX DS    0X                                                               
*======================================================================         
* OUTPUT MAP BOOK LIST                                                |         
*======================================================================         
                                                                                
OUTBOOKL DS    0X                                                               
R#BOOKL  EQU   8                   BOOK FOR STATION VALID LIST                  
         DC    AL2(OUTBOOKX-*)                                                  
         DC    AL2(R#BOOKL)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
                                                                                
J#BKFIL  EQU   12                  FILE                                         
                                                                                
         DC    AL2(J#BKFILX-*)                                                  
         DC    AL2(J#BKFIL),C'FILE '                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(J#BKFIL)                                                     
         DC    CL5'FILE '                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(MYFILE-SAVED),AL1(LD_CHARQ,3)                                
         DC    XL4'00'                                                          
J#BKFILX DS    0X                                                               
                                                                                
J#BOOKS  EQU   13                  BOOKS                                        
                                                                                
         DC    AL2(J#BOOKSX-*)                                                  
         DC    AL2(J#BOOKS),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(J#BOOKS)                                                     
         DC    CL5'BOOKS'                                                       
         DC    AL1(LO_IARRQ,0,LO_IXNUQ)                                         
         DC    AL2(ARYBKTY2-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
J#BOOKSX DS    0X                                                               
                                                                                
J#BOOK2X DS    0X                                                               
                                                                                
OUTBOOKX DS    0X                                                               
*======================================================================         
* OUTPUT MAP DEMOS LIST DOWNLOAD                                      |         
*======================================================================         
                                                                                
OUTDEMOL DS    0X                                                               
R#DEMOL  EQU   9                                                                
         DC    AL2(OUTDEMLX-*)                                                  
         DC    AL2(R#DEMOL)                                                     
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
J#DMCAT  EQU   15                  CATEGORY                                     
         DC    AL2(J#DMCATX-*)                                                  
         DC    AL2(J#DMCAT),C'DMCAT'                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(J#DMCAT)                                                     
         DC    CL5'DMCAT'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(DMCAT-SAVED),AL1(LD_CHARQ,8)                                 
         DC    XL4'00'                                                          
J#DMCATX DS    0X                                                               
J#DEMOS  EQU   16                  DEMOS                                        
         DC    AL2(J#DEMOX-*)                                                   
         DC    AL2(J#DEMOS),C'ARRAY'                                            
         DC    AL1(0,0,LO_ISETQ)                                                
         DC    XL4'00'                                                          
         DC    AL2(J#DEMOS)                                                     
         DC    CL5'DEMOS'                                                       
         DC    AL1(LO_IARRQ,0,LO_IXNUQ)                                         
         DC    AL2(ARYDEMOL-SVRDEF),AL1(0,0)                                    
         DC    XL4'00'                                                          
J#DEMOX  DS    0X                                                               
                                                                                
OUTDEMLX DS    0X                                                               
*======================================================================         
* OUTPUT MAP FOR UPGRADE VALIDATION                                   |         
*======================================================================         
                                                                                
OUTUPGD  DS    0X                                                               
R#UPGD   EQU   12                                                               
         DC    AL2(OUTUPGDX-*)                                                  
         DC    AL2(R#UPGD)                                                      
         DC    AL1(0),AL2(0)                                                    
         DC    XL4'00'                                                          
J#UPFLG  EQU   18                  UPGRADE FLAG                                 
         DC    AL2(J#UPFLGX-*)                                                  
         DC    AL2(J#UPFLG),C'UPFLG'                                            
         DC    AL1(0,0,0)                                                       
         DC    XL4'00'                                                          
         DC    AL2(J#UPFLG)                                                     
         DC    CL5'UPFLG'                                                       
         DC    AL1(B#SAVED,0,LO_IXNUQ)                                          
         DC    AL2(UPGRDFLG-SAVED),AL1(LD_CHARQ,1)                              
         DC    XL4'00'                                                          
J#UPFLGX DS    0X                                                               
                                                                                
OUTUPGDX DS    0X                                                               
*======================================================================         
* ARRAY DEFINITION BOOKTYPE DOWNLOAD                                  |         
*======================================================================         
                                                                                
ARYFSRC  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(FSRCVALS-SAVED)                                              
         DC    AL2(FSRCENUM-SAVED)                  # OF ROWS                   
         DC    AL2(FSRCVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(FSRCCOLN)                                                    
         DC    XL4'00'                                                          
FSRCCOL  DC    0X                                                               
                                                                                
DD#FILEC EQU   1                                   FILE CODE                    
         DC    AL2(DD#FILEC),C'FILEC'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(FILECODE-FSRCVALS),AL1(L'FILECODE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#FILED EQU   2                                   FILE DESCRIPTION             
         DC    AL2(DD#FILED),C'FILED'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(FILEDIS-FSRCVALS),AL1(L'FILEDIS)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SRCEC EQU   3                                   SOURCE CODE                  
         DC    AL2(DD#SRCEC),C'SRCEC'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SRCECODE-FSRCVALS),AL1(L'SRCECODE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#SRCED EQU   4                                   FILE DESCRIPTION             
         DC    AL2(DD#SRCED),C'SRCED'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(SRCEDIS-FSRCVALS),AL1(L'SRCEDIS)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
FSRCCOLN EQU   (*-FSRCCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION BOOKTYPE DOWNLOAD  FOR INTIAL DOWNLOAD             |         
*======================================================================         
                                                                                
ARYBKTYP DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(BKTYVALS-SAVED)                                              
         DC    AL2(BKTYPNUM-SAVED)                  # OF ROWS                   
         DC    AL2(BKTYVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(BKTYCOLN)                                                    
         DC    XL4'00'                                                          
BKTYCOL  DC    0X                                                               
                                                                                
DD#BKTYP EQU   1                                   BOOKTYPE                     
         DC    AL2(DD#BKTYP),C'BKTYP'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BOOKTYPE-BKTYVALS),AL1(L'BOOKTYPE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#BKTYD EQU   2                                   BOOKTYPE DESCRIPTION         
         DC    AL2(DD#BKTYD),C'BKTYD'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BKTYDISP-BKTYVALS),AL1(L'BKTYDISP)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
*                                                                               
DD#BKCAT EQU   3                                   P/E BOOK?                    
         DC    AL2(DD#BKCAT),C'BKCAT'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BKCAT-BKTYVALS),AL1(L'BKCAT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
BKTYCOLN EQU   (*-BKTYCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION BOOKTYPE DOWNLOAD                                  |         
*======================================================================         
                                                                                
ARYBKTY2 DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(BKTYVALS-SAVED)                                              
         DC    AL2(BKTYPNUM-SAVED)                  # OF ROWS                   
         DC    AL2(BKTYVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(BKTYCOL2N)                                                   
         DC    XL4'00'                                                          
BKTYCOL2 DC    0X                                                               
                                                                                
DD#BKTY2 EQU   1                                   BOOKTYPE                     
         DC    AL2(DD#BKTY2),C'BKTYP'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BOOKTYPE-BKTYVALS),AL1(L'BOOKTYPE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#BKRBK EQU   2                                   REAL BOOK                    
         DC    AL2(DD#BKRBK),C'RBOOK'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BKTYBOOK-BKTYVALS),AL1(L'BKTYBOOK)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#BKCAT2 EQU   3                                   P/E BOOK?                   
         DC    AL2(DD#BKCAT2),C'BKCAT'                                          
         DC    AL1(0,0,0)                                                       
         DC    AL2(BKCAT-BKTYVALS),AL1(L'BKCAT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#BKTYD2 EQU   4                                  BOOKTYPE DESCRIPTION         
         DC    AL2(DD#BKTYD2),C'BKTYD'                                          
         DC    AL1(0,0,0)                                                       
         DC    AL2(BKTYDISP-BKTYVALS),AL1(L'BKTYDISP)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#BKEND EQU   5                                   ENDDATE                      
         DC    AL2(DD#BKEND)                                                    
         DC    AL1(0,0,0,LX_CIMOQ,0)                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(BKENDDAT-BKTYVALS),AL1(L'BKENDDAT)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
BKTYCOL2N EQU   (*-BKTYCOL2)/LX_COLSL                                           
                                                                                
*======================================================================         
* ARRAY DEFINITION BOOKS    DOWNLOAD                                  |         
*======================================================================         
                                                                                
ARYB2TYP DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(B2TYVALS-SAVED)                                              
         DC    AL2(B2TYPNUM-SAVED)                  # OF ROWS                   
         DC    AL2(B2TYVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(B2TYCOLN)                                                    
         DC    XL4'00'                                                          
B2TYCOL  DC    0X                                                               
                                                                                
DD#B2TYP EQU   1                                   BOOKTYPE                     
         DC    AL2(DD#B2TYP),C'B2TYP'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(B2OKTYPE-B2TYVALS),AL1(L'B2OKTYPE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#B2TYD EQU   2                                   BOOKTYPE DESCRIPTION         
         DC    AL2(DD#B2TYD),C'B2TYD'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(B2TYDISP-B2TYVALS),AL1(L'B2TYDISP)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#B2CAT EQU   3                                   P/E BOOK?                    
         DC    AL2(DD#B2CAT),C'BKCAT'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(B2CAT-B2TYVALS),AL1(L'B2CAT)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
B2TYCOLN EQU   (*-B2TYCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION START-END TIME DOWNLOAD                            |         
*======================================================================         
                                                                                
ARYSETIM DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(SETMVALS-SAVED)                                              
         DC    AL2(SETIMNUM-SAVED)                  # OF ROWS                   
         DC    AL2(SETMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(SETMCOLN)                                                    
         DC    XL4'00'                                                          
SETMCOL  DC    0X                                                               
                                                                                
DD#STIME EQU   1                                     START TIME                 
         DC    AL2(DD#STIME),C'STIME'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(STIME-SETMVALS),AL1(L'STIME)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#ETIME EQU   2                                     END TIME                   
         DC    AL2(DD#ETIME),C'ETIME'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(ETIME-SETMVALS),AL1(L'ETIME)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#INTVL EQU   3                                     INTERVAL                   
         DC    AL2(DD#INTVL),C'INTVL'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(INTVL-SETMVALS),AL1(L'INTVL)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
SETMCOLN EQU   (*-SETMCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION DAYPART DOWNLOAD                                   |         
*======================================================================         
                                                                                
ARYDYPRT DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(DYPTVALS-SAVED)                                              
         DC    AL2(DYPRTNUM-SAVED)                  # OF ROWS                   
         DC    AL2(DYPTVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(DYPTCOLN)                                                    
         DC    XL4'00'                                                          
DYPTCOL  DC    0X                                                               
                                                                                
DD#DPRTS EQU   1                                    DAYPART CODE  SHORT         
         DC    AL2(DD#DPRTS),C'DYPTS'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DYPTCDES-DYPTVALS),AL1(L'DYPTCDES)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#DPRTL EQU   2                                    DAYPART CODE   LONG         
         DC    AL2(DD#DPRTL),C'DYPTL'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DYPTCDEL-DYPTVALS),AL1(L'DYPTCDEL)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#DYPTD EQU   3                                    DAYPART DESCRIPTION         
         DC    AL2(DD#DYPTD),C'DYPTD'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DYPTDISP-DYPTVALS),AL1(L'DYPTDISP)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DYPTCOLN EQU   (*-DYPTCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION STATIONS LIST DOWNLOAD                             |         
*======================================================================         
                                                                                
ARYSTAT  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED+LX_INERQ)                           
         DC    AL2(STATVALS-SAVED)                                              
         DC    AL2(STATNUM-SAVED)                  # OF ROWS                    
         DC    AL2(STATVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(STATCOLN)                                                    
         DC    XL4'00'                                                          
STATCOL  DC    0X                                                               
                                                                                
DD#STAT  EQU   1                                                                
         DC    AL2(DD#STAT),C'STAT '               STATION                      
         DC    AL1(0,0,0)                                                       
         DC    AL2(STATION-STATVALS),AL1(L'STATION)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#AFFIL EQU   2                                   AFFILIATION                  
         DC    AL2(DD#AFFIL),C'AFFIL'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(STATAFFL-STATVALS),AL1(L'STATAFFL)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#FLAG1 EQU   3                                   FLAG1                        
         DC    AL2(DD#FLAG1),C'FLAG1'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(STATFLAG-STATVALS),AL1(L'STATFLAG)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SBKTY EQU   4                                   BOOKTYPE                     
         DC    AL2(DD#SBKTY),C'BKTYP'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(STATBKTY-STATVALS),AL1(L'STATBKTY)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
STATCOLN EQU   (*-STATCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION STATIONS VALIDATION LIST DOWNLOAD                  |         
*======================================================================         
ARYVSTA  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(VSTAVALS-SAVED)                                              
         DC    AL2(VSTANUM-SAVED)                  # OF ROWS                    
         DC    AL2(VSTAVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(VSTACOLN)                                                    
         DC    XL4'00'                                                          
VSTACOL  DC    0X                                                               
DD#VSTA  EQU   1                                                                
         DC    AL2(DD#VSTA),C'STAT '               STATION                      
         DC    AL1(0,0,0)                                                       
         DC    AL2(VSTATION-VSTAVALS),AL1(L'VSTATION)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#SFLG  EQU   2                                   VALID STATION STATUS         
         DC    AL2(DD#SFLG),C'SFLAG'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(VSTAFLAG-VSTAVALS),AL1(L'VSTAFLAG)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#IVFLG EQU   3                                   HAVE INVENTORY FLAG          
         DC    AL2(DD#IVFLG),C'IFLAG'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(VINVFLAG-VSTAVALS),AL1(L'VINVFLAG)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#VBOOK EQU   4                                   BOOK STRING                  
         DC    AL2(DD#VBOOK),C'BOOK '                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(VSTABOOK-VSTAVALS),AL1(L'VSTABOOK)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#ALIAS EQU   5                                   BOOK ALIAS                   
         DC    AL2(DD#ALIAS),C'ALIAS'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(VSTALIAS-VSTAVALS),AL1(L'VSTALIAS)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#INDEX EQU   6                                   INDEX                        
         DC    AL2(DD#INDEX),C'INDEX'                                           
         DC    AL1(0,0,LX_IXNUQ)                                                
         DC    AL2(VSTINDX-VSTAVALS),AL1(L'VSTINDX)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
VSTACOLN EQU   (*-VSTACOL)/LX_COLSL                                             
                                                                                
                                                                                
*======================================================================         
* ARRAY DEFINITION MARKETS LIST DOWNLOAD                              |         
*======================================================================         
                                                                                
ARYMRKT  DS    0X                                                               
                                                                                
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED+LX_INERQ)                           
         DC    AL2(MRKTVALS-SAVED)                                              
         DC    AL2(MRKTNUM-SAVED)                  # OF ROWS                    
         DC    AL2(MRKTVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(MRKTCOLN)                                                    
         DC    XL4'00'                                                          
                                                                                
MRKTCOL  DC    0X                                                               
ED#AMKT  EQU   1                                   ALPHA MARKET                 
         DC    AL2(ED#AMKT),C'AMKT '                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(ALFMKT-MRKTVALS),AL1(L'ALFMKT)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
ED#NMKT  EQU   2                                   NUMERIC MARKET               
         DC    AL2(ED#NMKT),C'NMKT '                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(NUMMKT-MRKTVALS),AL1(L'NUMMKT)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
ED#MKTNM EQU   3                                   MARKET NAME                  
         DC    AL2(ED#MKTNM),C'MKTNM'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(MKTNAME-MRKTVALS),AL1(L'MKTNAME)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
MRKTCOLN EQU   (*-MRKTCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION DAY TIME AVG VALIDATION DOWNLOAD                   |         
*======================================================================         
                                                                                
ARYVDYTM DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(VDTMVALS-SAVED)                                              
         DC    AL2(VDTMNUM-SAVED)                  # OF ROWS                    
         DC    AL2(VDTMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(VDTMCOLN)                                                    
         DC    XL4'00'                                                          
VDYTMCOL DC    0X                                                               
DD#DAYTM EQU   1                                    DAYTIME                     
         DC    AL2(DD#DAYTM),C'DAYTM'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(VDAYTIME-VDTMVALS),AL1(L'VDAYTIME)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
DD#DTFLG EQU   2                                    FLAG                        
         DC    AL2(DD#DTFLG),C'DTFLG'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(VDTMFLAG-VDTMVALS),AL1(L'VDTMFLAG)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
VDTMCOLN EQU   (*-VDYTMCOL)/LX_COLSL                                            
                                                                                
*======================================================================         
* ARRAY DEFINITION DAYTIME DOWNLOAD                                   |         
*======================================================================         
                                                                                
ARYDYTIM DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(DYTMVALS-SAVED)                                              
         DC    AL2(DYTIMNUM-SAVED)                  # OF ROWS                   
         DC    AL2(DYTMVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(DYTMCOLN)                                                    
         DC    XL4'00'                                                          
DYTMCOL  DC    0X                                                               
DD#DYTMC EQU   1                                    DAYPART CODE                
         DC    AL2(DD#DYTMC),C'DYTMC'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DYTIMCDE-DYTMVALS),AL1(L'DYTIMCDE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#DYTMD EQU   2                                    DAYPART DES                 
         DC    AL2(DD#DYTMD),C'DYTMD'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DYTMDISP-DYTMVALS),AL1(L'DYTMDISP)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DYTMCOLN EQU   (*-DYTMCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION DEMOS VALIDATION DOWNLOAD                          |         
*======================================================================         
                                                                                
ARYDEMOS DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(DEMOVALS-SAVED)                                              
         DC    AL2(DEMOMNUM-SAVED)                  # OF ROWS                   
         DC    AL2(DEMOVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(DEMOCOLN)                                                    
         DC    XL4'00'                                                          
DEMOCOL  DC    0X                                                               
DD#DEMOS EQU   1                                    DEMOS                       
         DC    AL2(DD#DEMOS),C'DEMOS'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DEMOS-DEMOVALS),AL1(L'DEMOS)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#DMFLG EQU   2                                    FLAG                        
         DC    AL2(DD#DMFLG),C'DMFLG'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DEMOFLAG-DEMOVALS),AL1(L'DEMOFLAG)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DEMOCOLN EQU   (*-DEMOCOL)/LX_COLSL                                             
                                                                                
                                                                                
*======================================================================         
* ARRAY DEFINITION FOR BOOKS LIST DOWNLOAD                            |         
*======================================================================         
                                                                                
ARYBOOKS DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(BOOKVALS-SAVED)                                              
         DC    AL2(BOOKNUM-SAVED)                  # OF ROWS                    
         DC    AL2(BOOKVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(BOOKCOLN)                                                    
         DC    XL4'00'                                                          
                                                                                
BOOKCOL  DC    0X                                                               
ED#BOOKS EQU   1                                    BOOKS                       
         DC    AL2(ED#BOOKS),C'BOOKS'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BOOKS-BOOKVALS),AL1(L'BOOKS)                                 
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
                                                                                
ED#BKTYP EQU   2                                    BOOKTYPE                    
         DC    AL2(ED#BKTYP),C'BKTYP'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BKTYPE-BOOKVALS),AL1(L'BKTYPE)                               
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
BOOKCOLN EQU   (*-BOOKCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION DEMOS LIST DOWNLOAD                                |         
*======================================================================         
                                                                                
ARYDEMOL DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(DEMLVALS-SAVED)                                              
         DC    AL2(DEMOMNUM-SAVED)                  # OF ROWS                   
         DC    AL2(DEMLVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(DEMLCOLN)                                                    
         DC    XL4'00'                                                          
DEMLCOL  DC    0X                                                               
ED#DEMOS EQU   1                                    DEMOS                       
         DC    AL2(ED#DEMOS),C'DEMOS'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DEMLDEMO-DEMLVALS),AL1(L'DEMOS)                              
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
ED#DMNAM EQU   2                                    DEMO NAME                   
         DC    AL2(ED#DMNAM),C'DMNAM'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DEMONAME-DEMLVALS),AL1(L'DEMONAME)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
ED#DMTYP EQU   3                                    DEMO TYPES                  
         DC    AL2(ED#DMTYP),C'DMTYP'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DEMOTYPE-DEMLVALS),AL1(L'DEMOTYPE)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DEMLCOLN EQU   (*-DEMLCOL)/LX_COLSL                                             
                                                                                
*======================================================================         
* ARRAY DEFINITION DEMO MODIFIER DEFINITION                           |         
*======================================================================         
                                                                                
ARYDMMOD DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(DMODVALS-SAVED)                                              
         DC    AL2(DMODNUM-SAVED)                  # OF ROWS                    
         DC    AL2(DMODVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(DMODCOLN)                                                    
         DC    XL4'00'                                                          
DMODCOL  DC    0X                                                               
                                                                                
DD#DEMOD EQU   1                                   DEMO MODIFIER                
         DC    AL2(DD#DEMOD),C'DMMOD'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DEMOMOD-DMODVALS),AL1(L'DEMOMOD)                             
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#DMNAM EQU   2                                   DEMO MOD DESCRIPTION         
         DC    AL2(DD#DMNAM),C'DMNAM'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(DMMODNAM-DMODVALS),AL1(L'DMMODNAM)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DMODCOLN EQU   (*-DMODCOL)/LX_COLSL                                             
                                                                                
                                                                                
*======================================================================         
* ARRAY DEFINITION TP PASS BUFFER SIZE TO PC FOR REPORTS              |         
* PER TRANSACTION                                                     |         
*======================================================================         
                                                                                
ARYBUFFS DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(BUFFVALS-SAVED)                                              
         DC    AL2(BUFFNUM-SAVED)                  # OF ROWS                    
         DC    AL2(BUFFVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(BUFFCOLN)                                                    
         DC    XL4'00'                                                          
BUFFCOL  DC    0X                                                               
                                                                                
DD#DEMSZ EQU   1                                   DEMO RECORD SIZE             
         DC    AL2(DD#DEMSZ),C'DEMSZ'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFDEMO-BUFFVALS),AL1(L'BUFFDEMO)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#STASZ EQU   2                                   STATION COLUMN SIZE          
         DC    AL2(DD#STASZ),C'STASZ'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFSTAS-BUFFVALS),AL1(L'BUFFSTAS)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#SUMMS EQU   3                                   SUMMARY SIZE                 
         DC    AL2(DD#SUMMS),C'SUMMS'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFSUMM-BUFFVALS),AL1(L'BUFFSUMM)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#TOTSZ EQU   4                                   TOTAL BUFFER SIZE            
         DC    AL2(DD#TOTSZ),C'TOTSZ'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFTOTS-BUFFVALS),AL1(L'BUFFTOTS)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#PRJSZ EQU   5                                   PROJECTION SIZE              
         DC    AL2(DD#PRJSZ),C'PRJSZ'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFPROJ-BUFFVALS),AL1(L'BUFFPROJ)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#VBKSZ EQU   6                         VALIDATE BKS FOR STATION               
         DC    AL2(DD#VBKSZ),C'VBKSZ'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFVBKS-BUFFVALS),AL1(L'BUFFVBKS)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#VSTAT EQU   7                         VALIDATE STATION BUFF                  
         DC    AL2(DD#VSTAT),C'VSTAT'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFVSTA-BUFFVALS),AL1(L'BUFFVSTA)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#PROJN EQU   8                         MAX NUMBER OF PROJECTIONS              
         DC    AL2(DD#PROJN),C'PROJN'                                           
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFPRJN-BUFFVALS),AL1(L'BUFFPRJN)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
DD#PAVL  EQU   9                         PAV UPGRADE RECAL LIMIT                
         DC    AL2(DD#PAVL),C'PAV L'                                            
         DC    AL1(0,0,0)                                                       
         DC    AL2(BUFFPAVL-BUFFVALS),AL1(L'BUFFPAVL)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_UBINQ,0)                                                  
         DC    XL4'00'                                                          
BUFFCOLN EQU   (*-BUFFCOL)/LX_COLSL                                             
                                                                                
                                                                                
*======================================================================         
* ARRAY DEFINITION TO PASS PROFILE BITS TO PC                         |         
*======================================================================         
                                                                                
ARYPROF  DS    0X                                                               
         DC    AL1(B#SAVED,LX_IRADR,B#SAVED)                                    
         DC    AL2(PROFVALS-SAVED)                                              
         DC    AL2(PROFNUM-SAVED)                  # OF ROWS                    
         DC    AL2(PROFVALL)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(PROFCOLN)                                                    
         DC    XL4'00'                                                          
PROFCOL  DC    0X                                                               
                                                                                
DD#PHIST EQU   1                                   HISTORICAL                   
         DC    AL2(DD#PHIST),C'PHIST'              PROGRAM PROFILE              
         DC    AL1(0,0,0)                                                       
         DC    AL2(PROFHIST-PROFVALS),AL1(L'PROFHIST)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#SHPUT EQU   2                                   SHARE/PUT                    
         DC    AL2(DD#SHPUT),C'SHPUT'              PROFILE                      
         DC    AL1(0,0,0)                                                       
         DC    AL2(PROFSHPT-PROFVALS),AL1(L'PROFSHPT)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
DD#PJFRM EQU   3                                   PJ FORMULA                   
         DC    AL2(DD#PJFRM),C'PJFRM'              PROFILE                      
         DC    AL1(0,0,0)                                                       
         DC    AL2(PROFPJFM-PROFVALS),AL1(L'PROFPJFM)                           
         DC    AL2(0)                                                           
         DC    AL1(LD_CHARQ,0)                                                  
         DC    XL4'00'                                                          
PROFCOLN EQU   (*-PROFCOL)/LX_COLSL                                             
                                                                                
                                                                                
*======================================================================         
SAVED    DSECT                                                                  
                                                                                
REQVALS  DS    0A                                                               
                                                                                
BYTE     DS    X                                                                
BOOKIND  DS    XL1                                                              
ABOOK    DS    AL3                                                              
DAYIND   DS    XL1                                                              
ADAY     DS    AL3                                                              
DEMOIND  DS    XL1                                                              
ADEMO    DS    AL3                                                              
TIMEIND  DS    XL1                                                              
ATIME    DS    AL3                                                              
BKTYIND  DS    XL1                                                              
ABKTY    DS    AL3                                                              
FUNCIND  DS    XL1                                                              
AFUNCT   DS    AL3                                                              
FILEIND  DS    XL1                                                              
AFILE    DS    AL3                                                              
MEDIND   DS    XL1                                                              
AMED     DS    AL3                                                              
STAIND   DS    XL1                                                              
ASTAT    DS    AL3                                                              
AMKTIND  DS    XL1                                                              
AAMKT    DS    AL3                                                              
                                                                                
DYTMIND  DS    XL1                                                              
ADAYTIME DS    AL3                                                              
INDXIND  DS    XL1                                                              
AINDEX   DS    AL3                                                              
UPGDIND  DS    XL1                                                              
AUPGRD   DS    AL3                                                              
                                                                                
REQVALSL EQU   *-REQVALS                                                        
                                                                                
ELCODE   DS    C                                                                
DATADISP DS    H                                                                
FILEPTR  DS    A                                                                
SRCEPTR  DS    A                                                                
SETIMPTR DS    A                                                                
BKTYPPTR DS    A                                                                
BOOKPTR  DS    A                                                                
NUMBKS   DS    XL4                                                              
MRKTPTR  DS    A                                                                
SPILLFLG DS    C                                                                
BKERRFLG DS    X                                                                
TMPBOOK  DS    XL3                                                              
TMPBKTYP DS    C                                                                
TMP2CHBT DS    CL2               2 CHAR BOOKTYPE                                
TMPINPBK DS    CL40              MAX BOOK SIZE(MAY12/2001(H)-5)                 
TMPMULBK DS    XL6                                                              
TMPBKWK  DS    CL1               WEEK                                           
XSPLLFLG DS    CL1                                                              
VERSNUM  DS    XL4                                                              
VERS48   EQU   X'01000030'                                                      
AGYCODE  DS    CL2                                                              
TODAYDAT DS    CL8               TODAY'S DATE                                   
LSTWKDAT DS    CL8               LAST WEEK DATE                                 
TODAYBK  DS    XL2               TODAYS INTERNAL BOOK                           
LKUPBK   DS    XL2               LOOK UP BOOK                                   
LASTWKBK DS    XL2               LAST WEEKS BOOK                                
BKHKCNT  DS    X                 COUNT HOW MANY TIMES WE IN BOOK HOOK           
SVSELSTA DS    CL(L'DBSELSTA)                                                   
SVBKTYPE DS    XL1                                                              
ALPHADAY DS    CL3                                                              
ALPHADY2 DS    CL3                                                              
BIYSTRDY DS    CL1               BINARY YESTERDAY                               
BISELDAY DS    CL1               BINARY SELDAY                                  
FOUNDDAY DS    C                 Y/N                                            
                                                                                
PREVSTMK DS    XL7                                                              
DUMSTATH DS    CL8                                                              
DUMSTAT  DS    CL20                                                             
ALFMKTS  DS    CL3                                                              
SPILLMKT DS    XL2                                                              
MYFILE   DS    CL3                                                              
DMCAT    DS    CL15                                                             
UPGRDFLG DS    CL1                                                              
                                                                                
OUTVALS  DS    0X                                                               
MAXNDEM  DS    XL1                                                              
MAXBKSTN DS    XL1                                                              
PROJFLAG DS    CL19                                                             
OPTSPWKS DS    C                                                                
OPTMBKS  DS    C                                                                
FSRCVALS DS    0X                                                               
FILEDIS  DS    CL25                                                             
FILECODE DS    CL3                                                              
SRCECODE DS    CL5                                                              
SRCEDIS  DS    CL21                                                             
FSRCVALL EQU   *-FSRCVALS                                                       
FSRCENUM DS    AL2                                                              
                                                                                
BKTYVALS DS    0X                                                               
BKTYSEND DS    CL1                                                              
BOOKTYPE DS    CL5                                                              
*BKTYDISP DS   CL25                                                             
BKTYDISP DS    CL30                                                             
BKCAT    DS    CL1                               E/P BOOK?                      
BKTYBOOK DS    CL10                                                             
BKENDDAT DS    CL10     END DATE- USED REALLY FOR PROPOSER OVERNIGHTS           
BKTYVALL EQU   *-BKTYVALS                                                       
         DS    (MAXBTNUM)XL(BKTYVALL)                                           
BKTYPNUM DS    AL2                                                              
*                                                                               
B2TYVALS DS    0X                                                               
B2OKTYPE DS    CL5                                                              
B2TYDISP DS    CL30                                                             
B2CAT    DS    CL1                               E/P BOOK?                      
B2TYVALL EQU   *-B2TYVALS                                                       
B2TYPNUM DS    AL2                                                              
                                                                                
SETMVALS DS    0X                                                               
STIME    DS    AL2                                                              
ETIME    DS    AL2                                                              
INTVL    DS    AL1                                                              
SETMVALL EQU   *-SETMVALS                                                       
         DS    20XL(SETMVALL)                                                   
SETIMNUM DS    AL2                                                              
                                                                                
DYTIMNUM DS    AL2                                                              
DYTMVALS DS    0X                                                               
DYTMSEND DS    CL1                                                              
DYTIMCDE DS    CL6                                                              
DYTMDISP DS    CL25                                                             
DYTMVALL EQU   *-DYTMVALS                                                       
         DS    20XL(DYTMVALL)                                                   
                                                                                
DYPTVALS DS    0X                                                               
DYPTSEND DS    CL1                                                              
DYPTCDES DS    CL1                                                              
DYPTCDEL DS    CL3                                                              
DYPTDISP DS    CL15                                                             
DYPTVALL EQU   *-DYPTVALS                                                       
         DS    45XL(DYPTVALL)                                                   
DYPRTNUM DS    AL2                                                              
                                                                                
VDTMNUM  DS    AL2                                                              
VDTMVALS DS    0X                                                               
VDTMSEND DS    CL1                                                              
VDAYTIME DS    CL25                                                             
VDTMFLAG DS    CL1                                                              
VDTMVALL EQU   *-VDTMVALS                                                       
                                                                                
DEMOMNUM DS    AL2                                                              
DEMOVALS DS    0X                                                               
DEMOSEND DS    CL1                                                              
DEMOS    DS    CL20                                                             
DEMOFLAG DS    CL1                                                              
DEMOVALL EQU   *-DEMOVALS                                                       
                                                                                
DEMLVALS DS    0X                                                               
DEMLDEMO DS    CL20                                                             
DEMONAME DS    CL15                                                             
DEMOTYPE DS    CL8                                                              
                                                                                
DEMLVALL EQU   *-DEMLVALS                                                       
                                                                                
VSTANUM  DS    AL2                                                              
VSTAVALS DS    0X                                                               
VSTASEND DS    CL1                                                              
VSTATION DS    CL10                                                             
VSTAFLAG DS    CL1                                                              
VINVFLAG DS    CL1                                                              
VSTABOOK DS    CL40                                                             
VSTALIAS DS    CL8                      ALIAS NAME FOR INVENTORY BOOK           
VSTINDX  DS    XL1                                                              
VSTAVALL EQU   *-VSTAVALS                                                       
                                                                                
BOOKNUM  DS    AL2                                                              
BOOKVALS DS    0X                                                               
BOOKSEND DS    CL1                                                              
BOOKS    DS    CL10                                                             
BKTYPE   DS    CL1                                                              
BOOKVALL EQU   *-BOOKVALS                                                       
                                                                                
OUTVALSL EQU   *-OUTVALS                                                        
                                                                                
OUTVALS2 DS    0X                                                               
                                                                                
STATNUM  DS    AL2                                                              
STATVALS DS    0X                                                               
STATSEND DS    CL1                                                              
STATION  DS    CL10                                                             
STATAFFL DS    CL5                                                              
STATFLAG DS    CL1                                                              
*STATBKTY DS    CL1                                                             
STATBKTY DS    CL2                                                              
STATVALL EQU   *-STATVALS                                                       
                                                                                
MRKTNUM  DS    AL2                                                              
MRKTVALS DS    0X                                                               
MRKTSEND DS    CL1                                                              
ALFMKT   DS    CL4                                                              
NUMMKT   DS    CL4                                                              
MKTNAME  DS    CL30                                                             
MRKTVALL EQU   *-MRKTVALS                                                       
                                                                                
DMODNUM  DS    AL2                                                              
DMODVALS DS    0X                                                               
DMODSEND DS    CL1                                                              
DEMOMOD  DS    CL5                                                              
DMMODNAM DS    CL25                                                             
DMODVALL EQU   *-DMODVALS                                                       
         DS    5XL(DMODVALL)                                                    
*                                                                               
BUFFNUM  DS    AL2                                                              
BUFFVALS DS    0X                                                               
BUFFSEND DS    CL1                                                              
BUFFDEMO DS    XL4                                                              
BUFFSTAS DS    XL4                                                              
BUFFSUMM DS    XL4                                                              
BUFFTOTS DS    XL4                                                              
BUFFPROJ DS    XL4                                                              
BUFFVBKS DS    XL4                                                              
BUFFVSTA DS    XL4                                                              
BUFFPRJN DS    XL4                                                              
BUFFPAVL DS    XL4                                                              
BUFFVALL EQU   *-BUFFVALS                                                       
*                                                                               
PROFNUM  DS    AL2                                PROFILES                      
PROFVALS DS    0X                                                               
PROFSEND DS    CL1                                                              
PROFHIST DS    CL1                                                              
PROFPJFM DS    CL1                                                              
PROFSHPT DS    CL1                                                              
PROFVALL EQU   *-PROFVALS                                                       
                                                                                
OUTVAL2L EQU   *-OUTVALS2                                                       
                                                                                
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DELNKWRK                                                       
                                                                                
*                                                                               
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE DEDEMEQUS                                                      
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENIBKL                                                      
       ++INCLUDE DDSCANBLKD                                                     
                                                                                
RRDPRECD DSECT                                                                  
       ++INCLUDE REGENRDP                                                       
       ++INCLUDE FAFACTS                                                        
                                                                                
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMTABD                                                      
IOWORKD  DSECT                                                                  
IOWORK   DS    CL96                FOR GETREC/SPTFIL                            
IOPARM   DS    6F                  DATAMGR PARM LIST                            
IOWORK1  DS    X                   I/O COMMAND FLAG                             
IOWORK2  DS    X                   FILE/DIR NUMBER                              
IOWORK3  DS    X                   COMMAND NUMBER                               
IOFILE   DS    CL7                 I/O FILE NAME                                
IODIR    DS    CL7                 I/O DIRECTORY NAME                           
IOCMND   DS    CL7                 I/O DATAMGR COMMAND                          
IOINDS   DS    0XL5                I/O INDICATORS                               
IOFILTY  DS    XL1                 FILE/DIRECTORY TYPE                          
IOKEYLN  DS    XL1                 L'KEY (DANDX INCLUDES DBMINKEY)              
IODADSP  DS    XL1                 DISPLACEMENT TO D/A IN KEY                   
IOFSDSP  DS    XL1                 DISPLACEMENT TO FIL STATUS BYTE              
IODSDSP  DS    XL1                 DISPLACEMENT TO DIR STATUS BYTE              
IOWORKX  EQU   *                                                                
         SPACE 1                                                                
*******************************************************************             
STATTAB  DSECT                                                                  
         DS    CL1                                                              
MYSTCALL DS    CL10                                                             
MYMARALF DS    CL5                                                              
MYKBOOK  DS    XL8                                                              
MYKBTYP  DS    XL1                                                              
MYSPILLF DS    CL1                                                              
STATTABL EQU   *-STATTAB                                                        
*                                                                               
VBKSTAD  DSECT                                   DSECT TO COVER VALIDAT         
*                                                BOOKS FOR STATIONS             
VBKFILE  DS    CL3                               FILE CODE                      
VBKBKTY  DS    CL1                               BOOKTYPE                       
VBKWEEK  DS    CL1                               WEEK                           
VBKVBFLG DS    XL1                               VALID BOOK FLAG                
VBKBOOK  DS    XL3                               3 BYTE BOOK VALUE              
*VBKINBK  DS    CL15                             15 BYTES MAX BK STRING         
VBKMBK   DS    XL6                              3 MULTIBOOK                     
VBKINBK  DS    CL40                             15 BYTES MAX BK STRING          
VBKINSTA DS    CL11                              11 BYTES MAX INPUT STA         
VBKVSFLG DS    XL1                               VALID STATION FLAG             
VBKSTATL DS    XL1                               LEN OF STATION STRING          
VBKSPLLF DS    XL1                               SPILL MKT INDICATOR            
VBKINDX  DS    XL1                               INDEX                          
VBKSTADQ EQU   *-VBKSTAD                                                        
                                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031DELNK10   04/30/10'                                      
         END                                                                    
                                                                                
