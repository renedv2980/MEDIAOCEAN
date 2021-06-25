*          DATA SET SPSYN01    AT LEVEL 027 AS OF 05/01/02                      
*PHASE T21C01A,+0,NOAUTO                                                        
         TITLE 'T21C01 - SPOTPAK SYNDICATION - ADD/CHANGE'                      
         PRINT NOGEN                                                            
T21C01   CSECT                                                                  
         NMOD1 0,T21C01                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21CFFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING SYNRECD,R8                                                       
* READ RECORD                                                                   
         LA    R2,SYNACTH                                                       
         MVC   KEY(13),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    AC2                                                              
         CLI   SVACT,C'A'                                                       
         BE    *+6                                                              
         DC    H'0'                IT WAS THERE ON DISPLAY                      
         EJECT                                                                  
* BUILD NEW RECORD                                                              
         XC    SYNKEY(256),SYNKEY                                               
         MVC   SYNKEY,SVKEY                                                     
         MVC   SYNLEN,=H'36'                                                    
         MVC   SYNAGYA,AGYALPHA                                                 
         MVI   SVLIN,0                                                          
         LA    R4,SYNEL                                                         
         USING SYNACTEL,R4                                                      
         MVC   0(2,R4),=X'010C'                                                 
         BAS   R9,ACTODAY                                                       
         MVC   SYNACTCR,DUB                                                     
         B     AC3                                                              
*                                                                               
AC2      GOTO1 GETREC                                                           
*                                                                               
AC3      LA    R4,SYNEL                                                         
         USING SYNACTEL,R4                                                      
         BAS   R9,ACTODAY                                                       
         MVC   SYNACTDT,DUB                                                     
         CLC   =C'BF',SYNLIN                                                    
         BNE   AC4                                                              
         CLI   SVACT,C'A'                                                       
         BE    AC8                                                              
         B     AC70                                                             
*                                                                               
AC4      CLI   SVACT,C'A'          TEST ADD                                     
         BNE   AC10                                                             
* FIND HIGHEST LINE USED                                                        
         MVI   ELCODE,X'21'                                                     
         LA    R4,SYNEL                                                         
         USING SYNCVEL,R4                                                       
AC5      BAS   R9,NEXTEL                                                        
         BNE   AC6                                                              
         MVC   SVLIN,SYNCVLIN                                                   
         B     AC5                                                              
AC6      IC    RE,SVLIN                                                         
         LA    RE,1(RE)                                                         
         STC   RE,SVLIN                                                         
         B     AC10                                                             
         SPACE 2                                                                
AC8      LA    R4,SYNEL            ACTION IS ADD BF                             
         MVI   ELCODE,X'11'        MAKE SURE NONE THERE                         
         BAS   R9,NEXTEL                                                        
         BNE   AC70                                                             
         MVI   ERRCD,BFDUP                                                      
         B     ACERR                                                            
         EJECT                                                                  
ACTODAY  DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(1,DUB)                                       
         BR    R9                                                               
         EJECT                                                                  
* COVERAGE PERIOD                                                               
*                                                                               
AC10     XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING SYNCVEL,R4                                                       
         MVC   0(2,R4),=X'2128'                                                 
*                                                                               
         LA    R2,SYNCPERH                                                      
         GOTO1 ANY                                                              
*                                                                               
         CLI   SVACT,C'A'                                                       
         BE    *+14                                                             
         CLC   =C'DELETE',8(R2)                                                 
         BE    AC100                                                            
*                                                                               
         CLC   =C'NONE',8(R2)                                                   
         BE    AC15X                                                            
         BAS   R9,GETPER                                                        
         MVC   SYNCVST,DUB                                                      
         MVC   SYNCVND,DUB+3                                                    
* START MUST BE MONDAY                                                          
         MVI   ERRCD,SDAYERR                                                    
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   0(R1),1                                                          
         BNE   ACERR                                                            
* END DAY MUST BE SUNDAY                                                        
         MVI   ERRCD,EDAYERR                                                    
         GOTO1 VGETDAY,DMCB,WORK+6,DUB                                          
         CLI   0(R1),7                                                          
         BNE   ACERR                                                            
*                                                                               
         CLI   SVSTA,0             TEST MARKET INPUT                            
         BZ    AC15                YES - EDIT GOALS ONLY                        
* DAY/TIME                                                                      
         LA    R2,SYNCDAYH                                                      
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         MVC   SYNCVDAY,SYNCDAY                                                 
* POINTS                                                                        
         LA    R2,SYNCPTSH                                                      
         CLI   SVGOAL+1,X'FF'        TEST DOLLARS ONLY                          
         BNE   AC13A               NO                                           
         CLI   5(R2),0             SHOULDN'T HAVE ANY INPUT                     
         BE    AC13X               OK                                           
         MVI   ERRCD,INVERR                                                     
         B     ACERR                                                            
AC13A    DS    0H                                                               
         SR    R0,R0                                                            
         CLI   5(R2),0                                                          
         BE    AC13X                                                            
         MVI   ERRCD,INVERR                                                     
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    ACERR                                                            
         GOTO1 PACK                                                             
AC13X    ST    R0,SYNCVPTS                                                      
* DOLLARS                                                                       
         LA    R2,SYNCDOLH                                                      
         GOTO1 ANY                                                              
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R0)                                         
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),0                                                          
         BNE   ACERR                                                            
         L     R0,4(R1)                                                         
         ST    R0,SYNCVDOL                                                      
* SEGMENT PCT                                                                   
         MVI   ERRCD,INVERR                                                     
         MVC   SYNCVSEG,=H'10000'                                               
         LA    R2,SYNCSEGH                                                      
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         LTR   R0,R0                                                            
         BZ    AC15                                                             
         GOTO1 VCASHVAL,DMCB,8(R2),(R0)                                         
         CLI   0(R1),0                                                          
         BNE   ACERR                                                            
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    ACERR                                                            
         CH    R0,=H'10000'                                                     
         BH    ACERR                                                            
         STH   R0,SYNCVSEG                                                      
*                                                                               
AC15     MVI   SYNCVIPT,C'W'       FORCE WEEKLY                                 
* GOAL POINTS                                                                   
         MVI   ERRCD,INVERR                                                     
         LA    R2,SYNGPTSH                                                      
         CLI   5(R2),0                                                          
         BE    AC15B                                                            
         CLI   SVGOAL+1,X'FF'        TEST DOLLARS ONLY                          
         BE    ACERR                                                            
         GOTO1 PACK                                                             
         ST    R0,SYNGLPTS                                                      
* GOAL DOLLARS                                                                  
AC15B    LA    R2,SYNGDOLH                                                      
         CLI   5(R2),0                                                          
         BE    AC15X                                                            
         ZIC   R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R0)                                         
         MVC   SYNGLDOL,4(R1)                                                   
*                                                                               
AC15X    CLI   SVACT,C'A'          TEST ADD                                     
         BNE   AC20                                                             
         MVC   SYNCVPCT,SVSHR      MOVE MARKET SHARE TO ELEM                    
AC17     MVC   SYNCVLIN,SVLIN                                                   
* FIND INSERTION POINT (AT E-O-R)                                               
         LA    R4,SYNEL                                                         
         MVI   ELCODE,0                                                         
         BAS   R9,NEXTEL                                                        
         B     AC24                                                             
         EJECT                                                                  
*                                                                               
* DELETE EXISTING ELEMENT                                                       
*                                                                               
AC20     LA    R4,SYNEL                                                         
         MVI   ELCODE,X'21'                                                     
AC22     BAS   R9,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SYNCVLIN,SVLIN                                                   
         BNE   AC22                                                             
* PRESERVE SHARE AND LINE NUMBER IN NEW ELEM                                    
         LA    RE,ELEM+(SYNCVPCT-SYNCVEL)                                       
         MVC   0(2,RE),SYNCVPCT                                                 
         LA    RE,ELEM+(SYNCVLIN-SYNCVEL)                                       
         MVC   0(1,RE),SYNCVLIN                                                 
*                                                                               
         GOTO1 VRECUP,DMCB,(R8),(R4),0                                          
*                                                                               
* CHECK FOR OVERLAPPING COVERAGES                                               
*                                                                               
AC24     ST    R4,CVELADDR         SAVE COVERAGE ELEM ADDR                      
         OC    ELEM+4(3),ELEM+4    TEST NO DATA IN ELEM                         
         BZ    AC28                NONE - SKIP DATE TESTS                       
         MVI   ERRCD,CVOVLAP                                                    
*                                                                               
         LA    R4,SYNEL                                                         
         USING SYNCVEL,R4                                                       
         MVI   ELCODE,X'21'                                                     
AC26     BAS   R9,NEXTEL                                                        
         BNE   AC28                                                             
         LA    RE,SYNCVST-SYNCVEL+ELEM   THIS IS NEW ELEM START                 
         CLC   SYNCVND,0(RE)             DOES OLD END BEFORE NEW START          
         BL    AC26                                                             
         LA    RE,SYNCVND-SYNCVEL+ELEM   THIS IS NEW ELEM END                   
         CLC   SYNCVST,0(RE)             DOES OLD START AFTER NEW END           
         BH    AC26                                                             
         LA    R2,SYNCPERH         POSITION CURSOR                              
         B     ACERR                                                            
*                                                                               
AC28     L     R4,CVELADDR         GET INSERTION POINT ADDRESS                  
         GOTO1 VRECUP,DMCB,(R8),ELEM,(R4)                                       
*                                                                               
         CLI   SVSTA,0             TEST MARKET INPUT                            
         BZ    AC80                YES - DONE                                   
         EJECT                                                                  
* GUARANTEE DATA                                                                
*                                                                               
AC30     LA    R2,SYNGPERH                                                      
         CLI   5(R2),0                                                          
         BE    AC50                                                             
*                                                                               
         CLI   SVACT,C'A'                                                       
         BE    *+14                                                             
         CLC   =C'DELETE',8(R2)                                                 
         BE    AC34                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING SYNGREL,R4                                                       
         MVC   0(2,R4),=X'3110'                                                 
         BAS   R9,GETPER                                                        
         MVC   SYNGRST,DUB                                                      
         MVC   SYNGRND,DUB+3                                                    
* AMOUNT                                                                        
         LA    R2,SYNGAMTH                                                      
         GOTO1 ANY                                                              
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    AC32                                                             
*                                                                               
         GOTO1 PACK                                                             
         MH    R0,=H'100'                                                       
         ST    R0,SYNGRAMT                                                      
         MVI   SYNGRIPT,C'$'                                                    
         B     AC34                                                             
* PERCENT (OR INVALID)                                                          
AC32     MVI   ERRCD,INVERR                                                     
         LA    R6,8(R2)                                                         
         CLI   0(R6),C'0'                                                       
         BL    ACERR                                                            
         CLI   0(R6),C'9'                                                       
         BH    ACERR                                                            
         CLI   1(R6),C'0'                                                       
         BL    ACERR                                                            
         CLI   1(R6),C'9'                                                       
         BH    ACERR                                                            
         CLC   2(3,R6),=C'PCT'                                                  
         BNE   ACERR                                                            
         PACK  DUB,0(2,R6)                                                      
         CVB   R0,DUB                                                           
         ST    R0,SYNGRAMT                                                      
         MVI   SYNGRIPT,C'P'                                                    
         EJECT                                                                  
AC34     CLI   SVACT,C'A'                                                       
         BE    AC38                                                             
* DELETE EXISTING GUAR ELEM IF PRESENT                                          
         L     R4,CVELADDR                                                      
         MVI   ELCODE,X'21'        FIND NEXT CVEL (OR E-O-R)                    
         BAS   R9,NEXTEL                                                        
         LR    R5,R4                                                            
*                                                                               
         L     R4,CVELADDR                                                      
         MVI   ELCODE,X'31'                                                     
         BAS   R9,NEXTEL                                                        
         BNE   AC38                                                             
         CR    R4,R5               TEST APPLIES TO THIS CVEL                    
         BH    AC38                                                             
* FOUND IT - DELETE IT                                                          
         GOTO1 VRECUP,DMCB,(R8),(R4),0                                          
* PUT GREL AFTER CVEL                                                           
         CLC   =C'DELETE',8(R2)                                                 
         BE    AC50                                                             
*                                                                               
AC38     L     R4,CVELADDR                                                      
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         GOTO1 VRECUP,DMCB,(R8),ELEM,(R4)                                       
*                                                                               
         EJECT                                                                  
* COMMENTS                                                                      
*                                                                               
AC50     LA    R2,SYNCOM1H                                                      
         CLI   5(R2),0                                                          
         BE    AC80                                                             
         CLC   =C'PCT=',8(R2)                                                   
         BNE   AC51                                                             
* INPUT IS A COVERAGE PCT OVERRIDE                                              
         MVI   ERRCD,INVERR                                                     
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         SH    R0,=H'4'            ADJUST LEN FOR PCT=                          
         BZ    ACERR                                                            
*                                                                               
         GOTO1 VCASHVAL,DMCB,12(R2),(R0)                                        
         CLI   0(R1),0                                                          
         BNE   ACERR                                                            
         L     R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    ACERR                                                            
         C     R0,=F'999'                                                       
         BH    ACERR                                                            
* INSERT PCT IN CVEL                                                            
         L     R4,CVELADDR                                                      
         USING SYNCVEL,R4                                                       
         STH   R0,SYNCVPCT                                                      
         B     AC80                                                             
         EJECT                                                                  
AC51     DS    0H                                                               
*                                                                               
* DELETE EXISTING COMMENTS                                                      
         L     R4,CVELADDR                                                      
         MVI   ELCODE,X'21'        FIND NEXT CVEL (OR EOR)                      
         BAS   R9,NEXTEL                                                        
         LR    R5,R4               SAVE ITS ADDRESS                             
* NOW FIND COMMENTS                                                             
         L     R4,CVELADDR                                                      
         MVI   ELCODE,X'41'                                                     
         BAS   R9,NEXTEL                                                        
         CR    R4,R5               TEST COM APPLIES TO THIS CV                  
         BNL   AC52                NO-SKIP                                      
         GOTO1 VRECUP,DMCB,(R8),(R4),0                                          
* SEE IF MORE                                                                   
         CLI   0(R4),X'41'                                                      
         BNE   AC52                                                             
         GOTO1 (RF),(R1),,(R4)                                                  
*                                                                               
AC52     CLC   =C'DELETE',8(R2)                                                 
         BE    AC80                                                             
         BAS   RE,EDTCOM           BUILD ELEMENT IN ELEM                        
*                                                                               
         LA    R2,SYNCOM2H                                                      
         CLI   5(R2),0                                                          
         BE    AC80                                                             
         CLC   =C'DELETE',8(R2)                                                 
         BE    AC80                                                             
*                                                                               
         BAS   RE,EDTCOM                                                        
         B     AC80                                                             
         EJECT                                                                  
EDTCOM   NTR1                                                                   
*                                                                               
         MVC   ELEM,SPACES                                                      
         LA    R4,ELEM                                                          
         USING SYNCOMEL,R4                                                      
*                                                                               
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SYNCOM(0),8(R2) *EXECUTED*                                       
*                                                                               
         MVI   0(R4),X'41'                                                      
         LA    RE,3(RE)                                                         
* MAKE COMMENT LENGTH A MULTIPLE OF 4                                           
         LA    RE,3(RE)                                                         
         SRL   RE,2                                                             
         SLL   RE,2                                                             
         STC   RE,1(R4)                                                         
* FIND INSERTION POINT                                                          
         L     R4,CVELADDR                                                      
         MVI   ELCODE,X'21'        FIND NEXT CVEL                               
         BAS   R9,NEXTEL                                                        
         GOTO1 VRECUP,DMCB,(R8),ELEM,(R4)                                       
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
* BALANCE FORWARD DATA                                                          
*                                                                               
AC70     LA    R2,SYNBPERH                                                      
         CLI   5(R2),0                                                          
         BE    AC80                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING SYNBFEL,R4                                                       
         MVC   0(2,R4),=X'1110'                                                 
* DATE                                                                          
         MVI   ERRCD,INVERR                                                     
         GOTO1 VDATVAL,DMCB,8(R2),WORK                                          
         OC    0(4,R1),0(R1)                                                    
         BZ    ACERR                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,WORK,(3,DUB)                                        
         MVC   SYNBFDT,DUB                                                      
* POINTS                                                                        
         LA    R2,SYNBPTSH                                                      
         GOTO1 ANY                                                              
*                                                                               
         MVI   ERRCD,INVERR                                                     
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R0)                                         
         CLI   0(R1),0                                                          
         BNE   ACERR                                                            
*                                                                               
         L     R0,4(R1)            CASHVAL GIVES POINTS X 100                   
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         ST    R1,SYNBFPTS                                                      
* DOLLARS                                                                       
         LA    R2,SYNBDOLH                                                      
         GOTO1 ANY                                                              
         MVI   ERRCD,INVERR                                                     
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,8(R2),(R0)                                         
         CLI   0(R1),0                                                          
         BNE   ACERR                                                            
*                                                                               
         MVC   SYNBFDOL,4(R1)                                                   
         EJECT                                                                  
* TEST EXISTING BFEL                                                            
*                                                                               
         LA    R4,SYNEL                                                         
         MVI   ELCODE,X'11'                                                     
         BAS   R9,NEXTEL                                                        
         BNE   AC72                                                             
* DELETE IT                                                                     
         GOTO1 VRECUP,DMCB,(R8),(R4),0                                          
* ADD IT AFTER ACTVTY EL                                                        
AC72     LA    R4,SYNEL                                                         
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         GOTO1 VRECUP,DMCB,(R8),ELEM,(R4)                                       
         SPACE 2                                                                
AC80     L     RF,PUTREC                                                        
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         L     RF,ADDREC                                                        
*                                                                               
         GOTO1 (RF)                                                             
         B     EXIT                                                             
         EJECT                                                                  
* DELETE COVERAGE AND RELATED ELEMS IF                                          
* SUBSEQUENT TO BALANCE FORWARD                                                 
*                                                                               
AC100    XC    FULL,FULL                                                        
         LA    R4,SYNEL                                                         
         USING SYNBFEL,R4                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   R9,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   FULL(3),SYNBFDT                                                  
*                                                                               
         LA    R4,SYNEL                                                         
         MVI   ELCODE,X'21'                                                     
         USING SYNCVEL,R4                                                       
AC102    BAS   R9,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SYNCVLIN,SVLIN                                                   
         BNE   AC102                                                            
         MVI   ERRCD,BFCVERR                                                    
         CLC   SYNCVST,FULL        CVELEM MUST START AFTER BF DATE              
         BL    ACERR                                                            
*                                                                               
AC104    GOTO1 VRECUP,DMCB,(R8),(R4),0                                          
*                                                                               
         CLI   0(R4),0             END OF REC                                   
         BE    AC106                                                            
         CLI   0(R4),X'21'         NEXT CV ELEM                                 
         BNE   AC104               DELETE THIS ONE TOO                          
*                                                                               
AC106    GOTO1 PUTREC                                                           
         MVC   SYNMSG(21),=C'COVERAGE LINE DELETED'                             
         MVI   SVLIN,0                                                          
         MVI   ERRAREA,X'FF'       SET FLAG TO INHIBIT DISPLAY                  
         XC    SYNLIN,SYNLIN                                                    
         FOUT  SYNLINH                                                          
         LA    R2,SYNPRH                                                        
         B     EXIT                                                             
         EJECT                                                                  
GETPER   LA    R6,8(R2)                                                         
         MVI   ERRCD,DTERR                                                      
         GOTO1 VDATVAL,DMCB,(R6),WORK                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    ACERR                                                            
         A     R6,0(R1)                                                         
         LA    R6,1(R6)                                                         
         GOTO1 (RF),(R1),(R6),WORK+6                                            
         OC    0(4,R1),0(R1)                                                    
         BZ    ACERR                                                            
         MVI   ERRCD,STNDERR                                                    
         CLC   WORK(6),WORK+6                                                   
         BH    ACERR                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,WORK,(3,DUB)                                        
*                                                                               
         GOTO1 VDATCON,DMCB,WORK+6,(3,DUB+3)                                    
*                                                                               
         BR    R9                                                               
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    NEXTELX                                                          
         CLC   0(1,R4),ELCODE                                                   
         BER   R9                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   R9,R9               SET CC NOT EQUAL                             
         BR    R9                                                               
         EJECT                                                                  
ACERR    GOTO1 ERROR                                                            
*                                                                               
EXIT     OI    6(R2),X'40'         INSERT CURSOR                                
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* SPSYNWRK                                                                      
       ++INCLUDE SPSYNWRK                                                       
         EJECT                                                                  
RECD     DSECT                                                                  
* SPGENSYN                                                                      
       ++INCLUDE SPGENSYN                                                       
 END                                                                            
