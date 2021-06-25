*          DATA SET RESFM18    AT LEVEL 002 AS OF 03/21/94                      
*PHASE T81818A                                                                  
         TITLE 'T81818 - RESFM18 - FORECAST REPORT'                             
***********************************************************************         
*                                                                     *         
*  RESFM18 (T81818) --- REPORT OF FORECAST RECORDS                    *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 08FEB94 (SKU) NEW PROGRAM                                           *         
*                                                                     *         
***********************************************************************         
T81818   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T81818*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                  ALL FILTERS OPTIONAL                         
         XC    FILTSTAT,FILTSTAT                                                
         XC    FILTADV,FILTADV                                                  
         XC    FILTOFF,FILTOFF                                                  
         XC    FILTPER,FILTPER                                                  
                                                                                
         LA    R2,REPSTAH          STATION                                      
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         GOTO1 VALISTA                                                          
         MVC   FILTSTAT,WORK                                                    
                                                                                
VK10     DS    0H                                                               
         LA    R2,REPADVH          ADVERTISER                                   
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         GOTO1 VALIADV                                                          
         MVC   FILTADV,WORK                                                     
                                                                                
VK20     DS    0H                                                               
         LA    R2,REPOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VK40                                                             
                                                                                
         CLI   TWAOFFC,C'*'       DDS TERMINAL?                                 
         BE    VK30               YES                                           
         CLC   =C'O=',TWAACCS     CHECK OFFICE RESTRICTION                      
         BNE   VK30                                                             
         CLC   TWAACCS+2(2),8(R2)                                               
         BNE   INVLOFF            OFFICE MUST MATCH                             
                                                                                
VK30     DS    0H                                                               
         GOTO1 VALIOFF                                                          
         MVC   FILTOFF,WORK                                                     
                                                                                
VK40     DS    0H                                                               
         LA    R2,REPPERH          PERIOD                                       
         CLI   5(R2),0                                                          
         BE    VKX                                                              
                                                                                
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB,DMCB                                                        
         BZ    INVLFLD                                                          
*                                                                               
* ONLY VALID PERIODS ARE JAN/APR/JUL AND OCT                                    
*                                                                               
         CLC   =C'01',WORK+2                                                    
         BE    VK50                                                             
         CLC   =C'04',WORK+2                                                    
         BE    VK50                                                             
         CLC   =C'07',WORK+2                                                    
         BE    VK50                                                             
         CLC   =C'10',WORK+2                                                    
         BNE   INVLPER                                                          
                                                                                
VK50     DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,FILTPER)                                 
                                                                                
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS REPORT                                                                
***********************************************************************         
PR       DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
                                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSBBKEY,R6                                                       
         MVI   RSBBKTYP,X'2D'                                                   
         MVC   RSBBKREP,AGENCY                                                  
                                                                                
         OC    FILTSTAT,FILTSTAT   FILTER ON STATION?                           
         BZ    PR10                                                             
                                                                                
         MVC   RSBBKSTA,FILTSTAT   YES, START WITH THIS STATION                 
         DROP  R6                                                               
                                                                                
PR10     DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
PR20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSBBKEY,R6                                                       
         CLI   RSBBKTYP,X'2D'                                                   
         BNE   PRX                                                              
         CLC   RSBBKREP,AGENCY                                                  
         BNE   PRX                                                              
                                                                                
         OC    FILTSTAT,FILTSTAT   FILTER ON STATION?                           
         BZ    PR30                                                             
         CLC   RSBBKSTA,FILTSTAT                                                
         BNE   PRX                                                              
                                                                                
PR30     DS    0H                                                               
         OC    FILTADV,FILTADV     FILTER ON ADVERTISER?                        
         BZ    PR40                                                             
         CLC   RSBBKADV,FILTADV                                                 
         BNE   PRSEQ                                                            
                                                                                
PR40     DS    0H                                                               
         OC    FILTOFF,FILTOFF     FILTER ON OFFICE?                            
         BZ    PR50                                                             
         CLC   RSBBKOFF,FILTOFF                                                 
         BNE   PRSEQ                                                            
                                                                                
PR50     DS    0H                                                               
         OC    FILTPER,FILTPER     FILTER ON PERIOD?                            
         BZ    PR60                                                             
         CLC   RSBBKPER,FILTPER                                                 
         BNE   PRSEQ                                                            
         DROP  R6                                                               
                                                                                
PR60     DS    0H                                                               
         GOTO1 GETREC                                                           
                                                                                
PR70     DS    0H                                                               
         L     R6,AIO                                                           
         USING RSBBREC,R6                                                       
                                                                                
         CLI   LINE,8              SHOW ALL FOR FIRST ENTRY ON PAGE             
         BNE   PR80                                                             
         XC    SVSTA(11),SVSTA                                                  
                                                                                
PR80     DS    0H                                                               
         OC    SVSTA,SVSTA         IF SAME STATION, DON'T PRINT STATION         
         BZ    PR90                                                             
         CLC   RSBBKSTA,SVSTA                                                   
         BE    PR110                                                            
PR90     MVC   PSTA(4),RSBBKSTA                                                 
         CLI   RSBBKSTA+4,C' '                                                  
         BE    PR100                                                            
         MVC   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),RSBBKSTA+4                                             
PR100    MVC   SVSTA,RSBBKSTA                                                   
         XC    SVADV(6),SVADV                                                   
                                                                                
PR110    DS    0H                  IF SAME ADV, DON'T PRINT ADV                 
         OC    SVADV,SVADV                                                      
         BZ    PR120                                                            
         CLC   RSBBKADV,SVADV                                                   
         BE    PR130                                                            
PR120    MVC   PADV,RSBBKADV                                                    
         MVC   SVADV,RSBBKADV                                                   
         XC    SVOFF,SVOFF                                                      
                                                                                
PR130    DS    0H                  IF SAME OFF, DON'T PRINT OFFICE              
         OC    SVOFF,SVOFF                                                      
         BZ    PR140                                                            
         CLC   RSBBKOFF,SVOFF                                                   
         BE    PR150                                                            
PR140    MVC   POFF,RSBBKOFF                                                    
         MVC   SVOFF,RSBBKOFF                                                   
                                                                                
PR150    DS    0H                  ALWAYS PRINT THE PERIOD                      
         GOTO1 DATCON,DMCB,(2,RSBBKPER),(6,PPER)                                
         DROP  R6                                                               
                                                                                
PR160    DS    0H                                                               
         L     R6,AIO              PRINT FORECAST AMOUNTS                       
         USING RSBBELEM,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR190                                                            
                                                                                
         BAS   RE,CALCAMTS         CALCULATE THE STA/MKT TOTAL/SHRS             
                                                                                
         OC    TOTSTA,TOTSTA                                                    
         BZ    PR170                                                            
         OC    RSBBTOT,RSBBTOT                                                  
         BNZ   *+8                                                              
         MVI   PSTATOT-1,C'*'      CALCULATED                                   
         EDIT  TOTSTA,(12,PSTATOT),ALIGN=LEFT,ZERO=BLANK                        
                                                                                
PR170    DS    0H                                                               
         OC    TOTMKT,TOTMKT                                                    
         BZ    PR180                                                            
         OC    RSBBMTOT,RSBBMTOT                                                
         BNZ   *+8                                                              
         MVI   PMKTTOT-1,C'*'      CALCULATED                                   
         EDIT  TOTMKT,(12,PMKTTOT),ALIGN=LEFT,ZERO=BLANK                        
                                                                                
PR180    DS    0H                                                               
         OC    TOTSHR,TOTSHR                                                    
         BZ    PR190                                                            
         OC    RSBBSHR,RSBBSHR                                                  
         BNZ   *+8                                                              
         MVI   PSHR-1,C'*'         CALCULATED                                   
         EDIT  TOTSHR,(3,PSHR),ALIGN=LEFT,ZERO=BLANK                            
         DROP  R6                                                               
                                                                                
PR190    DS    0H                  PRINT FORECAST COMMENTS                      
         L     R6,AIO                                                           
         USING RSBBCTEL,R6                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    PR200                                                            
                                                                                
         BAS   RE,PRINT                                                         
         B     PR260                                                            
                                                                                
PR200    DS    0H                                                               
         CLC   RSBBCMT,SPACES                                                   
         BE    PR210                                                            
         OC    RSBBCMT,RSBBCMT                                                  
         BNZ   PR220                                                            
                                                                                
PR210    DS    0H                                                               
         BAS   RE,PRINT                                                         
         B     PR260                                                            
                                                                                
PR220    DS    0H                                                               
         CLI   LINE,55                                                          
         BL    PR240                                                            
                                                                                
         MVI   FORCEHED,C'Y'       NEW PAGE, DISPLAY ALL                        
         MVC   PSTA(4),SVSTA                                                    
         CLI   SVSTA+4,C' '                                                     
         BE    PR230                                                            
         MVC   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),SVSTA+4                                                
PR230    MVC   PADV,SVADV                                                       
         MVC   POFF,SVOFF                                                       
                                                                                
PR240    DS    0H                                                               
         BAS   RE,PRINT                                                         
         LA    R5,RSBBCMT                                                       
         LA    R4,3                                                             
                                                                                
PR250    DS    0H                                                               
         MVC   P+38(35),0(R5)                                                   
         BAS   RE,PRINT                                                         
         LA    R5,35(R5)                                                        
         BCT   R4,PR250                                                         
         DROP  R6                                                               
                                                                                
PR260    DS    0H                                                               
         BAS   RE,PRINT                                                         
                                                                                
PRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     PR20                                                             
                                                                                
PRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CALCULATE MARKET TOTAL, FORECAST TOTAL OR STATION SHARE                       
* MARKET TOTAL * STATION SHARE = STATION TOTAL                                  
***********************************************************************         
CALCAMTS NTR1                                                                   
         L     R6,AIO                                                           
         USING RSBBREC,R6                                                       
         MVC   TOTSTA,RSBBTOT                                                   
         MVC   TOTMKT,RSBBMTOT                                                  
         MVC   TOTSHR,RSBBSHR                                                   
         DROP  R6                                                               
                                                                                
         OC    TOTSTA,TOTSTA       STATION HAS VALUE                            
         BZ    CALC100                                                          
         OC    TOTMKT,TOTMKT       MARKET HAS NO VALUE                          
         BNZ   CALC200                                                          
         OC    TOTSHR,TOTSHR       STATION SHARE HAS VALUE                      
         BZ    CALCX               THEREFORE, CALCULATE MARKET TOTAL            
                                                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,TOTSTA                                                     
         MH    RF,=H'100'                                                       
         XC    WORK,WORK                                                        
         MVC   WORK+3,TOTSHR                                                    
         D     RE,WORK                                                          
         STCM  RF,15,TOTMKT                                                     
         B     CALCX                                                            
                                                                                
CALC100  DS    0H                  CALCULATE STATION TOTAL                      
         OC    TOTMKT,TOTMKT       MARKET HAS VALUE                             
         BZ    CALCX                                                            
         OC    TOTSHR,TOTSHR       STATION SHARE HAS VALUE                      
         BZ    CALCX               THEREFORE, CALCULATE STATION TOTAL           
                                                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,TOTMKT                                                     
         XC    WORK,WORK                                                        
         MVC   WORK+1(1),TOTSHR                                                 
         MH    RF,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK+3,100                                                       
         D     RE,WORK                                                          
         STCM  RF,15,TOTSTA                                                     
         B     CALCX                                                            
                                                                                
CALC200  DS    0H                  CALCULATE STATION SHARE                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,TOTSTA                                                     
         MH    RF,=H'100'                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(4),TOTMKT                                                   
         D     RE,WORK                                                          
         STC   RF,TOTSHR                                                        
         B     CALCX                                                            
                                                                                
CALCX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
INVLOFF  MVC   RERROR,=AL2(55)     OFFICE MUST MATCH                            
         B     ERREND                                                           
*                                                                               
INVLPER  MVC   RERROR,=AL2(169)    INVALID DATE                                 
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
         EJECT                                                                  
***********************************************************************         
* REPORT HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
         MVC   H4(7),=C'STATION'                                                
         MVC   H5(7),=11C'-'                                                    
         MVC   H4+9(10),=C'ADVERTISER'                                          
         MVC   H5+9(10),=11C'-'                                                 
         MVC   H4+21(6),=C'OFFICE'                                              
         MVC   H5+21(6),=11C'-'                                                 
         MVC   H4+29(6),=C'PERIOD'                                              
         MVC   H5+29(6),=11C'-'                                                 
         MVC   H4+38(11),=C'STATION TOT'                                        
         MVC   H5+38(11),=11C'-'                                                
         MVC   H4+52(10),=C'MARKET TOT'                                         
         MVC   H5+52(10),=11C'-'                                                
         MVC   H4+66(11),=C'STATION SHR'                                        
         MVC   H5+66(11),=11C'-'                                                
HOOKX    B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PADV     DS    CL4                                                              
         DS    CL8                                                              
POFF     DS    CL2                                                              
         DS    CL6                                                              
PPER     DS    CL6                                                              
         DS    CL3                                                              
PSTATOT  DS    CL12                                                             
         DS    CL2                                                              
PMKTTOT  DS    CL12                                                             
         DS    CL2                                                              
PSHR     DS    CL3                                                              
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMF2D          (OUR REPORT SCREEN OVERLAY)                  
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENSBB                                                       
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
FILTSTAT DS    CL5                                                              
FILTADV  DS    CL4                                                              
FILTOFF  DS    CL2                                                              
FILTPER  DS    CL2                                                              
SVSTA    DS    CL5                                                              
SVADV    DS    CL4                                                              
SVOFF    DS    CL2                                                              
TOTSTA   DS    XL4                                                              
TOTMKT   DS    XL4                                                              
TOTSHR   DS    XL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002RESFM18   03/21/94'                                      
         END                                                                    
