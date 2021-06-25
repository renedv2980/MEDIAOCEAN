*          DATA SET ACINQ0E    AT LEVEL 065 AS OF 05/01/02                      
*PHASE T6060EA,*,NOAUTO                                                         
*INCLUDE ACJOBCOL                                                               
         TITLE 'ACCOUNT ENQUIRY MK2 - ORDER SUMMARY - T6060E'                   
T6060E   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR TYPE OS IN ACCOUNT ENQUIRY PROGRAM                    
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T6060E)                                               
         DC    A(FILTABLE-T6060E)                                               
         DC    A(KNTRYPNT-T6060E)                                               
         DC    A(FNTRYPNT-T6060E)                                               
         DC    A(DNTRYPNT-T6060E)                                               
         DS    A                                                                
         DS    A                                                                
*                                                                               
         EJECT                                                                  
*              KEYS TABLE COVERED BY DSECT KEYTABD                              
         SPACE 1                                                                
KEYTABLE DC    CL10'COMPANY'                                                    
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(MYCO-GWS)                                                    
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'UNIT'                                                       
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(1)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODUNIT-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'LEDGER'                                                     
         DC    C' '                                                             
         DC    C' '                                                             
         DC    X'00'                                                            
         DC    AL1(2)                                                           
         DC    AL1(1)                                                           
         DC    AL2(PRODLEDG-GWS)                                                
         DC    AL2(EDITUNLE-GWS)                                                
*                                                                               
         DC    CL10'ACCOUNT'                                                    
         DC    C'O'                                                             
         DC    C'V'                                                             
         DC    X'41'                                                            
         DC    AL1(3)                                                           
         DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
FILTABLE DC    CL10'CLOSED'                                                     
         DC    CL2'CL'                                                          
         DC    X'00'                                                            
         DC    CL8'YES/NO'                                                      
         DC    X'44'                                                            
         DC    AL2(ASTA-GWS)                                                    
         DC    AL1(ACSTSTAT-ACSTATD)                                            
         DC    AL1(L'ACSTSTAT)                                                  
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'MEDIA'                                                      
         DC    CL2'ME'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AKEY-GWS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    X'FF00'                                                          
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'OFFICE'                                                     
         DC    CL2'OF'                                                          
         DC    X'00'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(AOFFICE-GWS)                                                 
         DC    AL1(0)                                                           
         DC    AL1(1)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
*                                                                               
         DC    CL10'ORDERS'                                                     
         DC    CL2'OR'                                                          
         DC    X'00'                                                            
         DC    CL8'YES'                                                         
         DC    X'01'                                                            
         DC    AL2(0)                                                           
         DS    6C                                                               
*                                                                               
         DC    CL10'OVERRIDE'                                                   
         DC    CL2'OV'                                                          
         DC    X'80'                                                            
         DC    CL8' '                                                           
         DC    X'00'                                                            
         DC    AL2(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(3)                                                           
         DC    AL2(EDITOVER-GWS)                                                
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF FILTER TABLE                          
*                                                                               
KNTRYPNT DS    0H                                                               
         EJECT                                                                  
         SPACE 1                                                                
FNTRYPNT DS    0D                                                               
         NMOD1 0,**INQE**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060E,RB                                                        
         USING T606TWA,RA                                                       
         USING FILTERSD,R4                                                      
         ZIC   R1,SAVEHIER+2       GET DISPLACEMENT INTO KEY OF MEDIA           
         LA    R1,3(R1)                                                         
         STC   R1,FILDISP                                                       
         B     TXIT                                                             
         EJECT                                                                  
*              MAIN PROCESS                                                     
         SPACE 1                                                                
DNTRYPNT DS    0D                                                               
         NMOD1 0,**INQE**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T6060E,RB                                                        
         USING T606TWA,RA                                                       
         L     R7,ALOCAL                                                        
         USING LOCALD,R7                                                        
         ST    R2,MYRELO                                                        
         SPACE 1                                                                
T00A     OC    ACOLIST,ACOLIST                                                  
         BNZ   *+8                                                              
         BAS   RE,INITIAL                                                       
*                                                                               
         CLI   VIRGIN,C'H'         CLEAR GRAND TOTALS IF NO HITS                
         BE    T00B                                                             
         CLI   LINE+1,0            UNLESS ITS A CONTINUATION SCREEN             
         BNE   T00B                                                             
         ZAP   GTOTORD,=P'0'                                                    
         ZAP   GTOTEST,=P'0'                                                    
         ZAP   GTOTCHA,=P'0'                                                    
         ZAP   GTOTBIL,=P'0'                                                    
         SPACE 1                                                                
T00B     L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         CLI   ACKEYACC,RUNLAST    END                                          
         BE    T1                                                               
         CLI   ACKEYCON,C' '       IS IT AN A/C REC                             
         BNE   T07                 NO                                           
         B     T01                 YES                                          
         EJECT                                                                  
*              HANDLE JOB RECORD                                                
         SPACE 1                                                                
T01      CLC   ACKEYACC+1(2),PRODUNIT         MUST BE AN SJ ACCNT               
         BNE   TEND                                                             
         CLI   LEVEL+1,3           JOB RECORD                                   
         BL    TNEXT                                                            
         TM    OPTIONS,JOBTOTS     IS JOB DISPLAY PENDING                       
         BO    T1                  YES                                          
         NI    OPTIONS,X'FF'-JOBINUSE                                           
         SPACE 1                                                                
T03      CLC   RECOUNT,LIMIT       IS RECORD READING LIMIT EXCEEDED             
         BH    T8                  YES                                          
         SPACE 1                                                                
T04      DS    0H                  APPLY FILTERS                                
         MVI   DMCB,0                                                           
         GOTO1 AFILTER                                                          
         BZ    TNEXTAC                                                          
         ICM   R5,15,ABAL                                                       
         BZ    TNEXTAC                                                          
         USING ACBALD,R5                                                        
         ZAP   JOBORD,=P'0'        UNMATCHED ORD                                
         ZAP   JOBEST,=P'0'        PRESENT ESTIMATE                             
         L     RE,AIO                                                           
         USING ACKEYD,RE                                                        
         LH    RF,ACLENGTH                                                      
         DROP  RE                                                               
         LR    R1,RF                                                            
         L     R0,AIOB                                                          
         ST    R0,DMCB                                                          
         MVCL  R0,RE                                                            
         GOTO1 ASETELAD                                                         
         ICM   R5,15,ABAL                                                       
         ST    R5,ASAVEL                                                        
         OI    OPTIONS,JOBINUSE                                                 
         TM    OPTIONS,ONLYORDS    IF WE DONT REQUIRE ORDERS                    
         BO    T06                 CHECK NOW FOR ROOM ON SCREEN                 
         ICM   R4,15,ANAM                                                       
         BZ    T05                                                              
         USING ACNAMED,R4                                                       
         CLI   ACNMLEN,22                                                       
         BH    *+12                                                             
         CLI   LINE+1,20                                                        
         B     *+8                                                              
         CLI   LINE+1,19                                                        
         BNL   TFULL                                                            
T05      OI    OPTIONS,JOBTOTS     INDICATE JOB DISPLAY PENDING                 
         SPACE 1                                                                
T06      DS    0H                  ENSURE SEQUENTIAL READING                    
         XC    KEYMASK+15(27),KEYMASK+15                                        
         XC    KEYCHK+15(27),KEYCHK+15                                          
         B     TNEXT                                                            
         EJECT                                                                  
*              HANDLE A TRANSACTION                                             
         SPACE 1                                                                
T07      L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         CLC   ACKEYWRK,=C'**'     MUST BE WORKCODE ** (ORDER)                  
         BH    TNEXTAC                                                          
         CLI   ACKEYDTE,C' '       MUST BE TRANSACTION                          
         BE    TNEXT               CAC                                          
         TM    ACSTATUS,X'A0'      IGNORE DELETED ORDERS                        
         BO    TNEXT                                                            
         OI    OPTIONS,JOBTOTS                                                  
         ICM   R5,15,ASAVEL                                                     
         ICM   R4,15,AOAM                                                       
         BZ    TNEXT                                                            
         USING ACOAMTD,R4                                                       
         SR    R0,R0                                                            
         SPACE 1                                                                
T08      AP    JOBORD,ACOAMT       ADD UNMATCHED ORDS                           
         SP    JOBORD,ACOAIVAL                                                  
         IC    R0,ACOALEN                                                       
         AR    R4,R0                                                            
         CLI   ACOAEL,X'68'                                                     
         BE    T08                                                              
         B     TNEXT                                                            
         EJECT                                                                  
*              HANDLE DISPLAY FROM SAVED JOB RECORD IN IOB                      
T1       CLI   VIRGIN,C'H'                                                      
         BE    T2                                                               
         MVI   VIRGIN,C'H'                                                      
         CLI   LINE+1,3                                                         
         BNL   T2                                                               
         MVC   INFDAT2,HEADING1    FIRST TIME SET UP HEADINGS                   
         MVC   INFDAT3,HEADING2                                                 
         OI    INFDAT2H+6,X'80'                                                 
         OI    INFDAT3H+6,X'80'                                                 
         MVI   LINE+1,3                                                         
         B     T2                                                               
HEADING1 DC    CL39'JOB CODE     JOB NAME              UNMA'                    
         DC    CL39'TCHED    PRESENT     ACTUAL    BILLING '                    
HEADING2 DC    CL39'--------     --------                 O'                    
         DC    CL39'RDERS   ESTIMATE    CHARGES    ------- '                    
         SPACE 1                                                                
T2       LH    R6,LINE             SET UP DISPLAY FOR A RECORD                  
         LR    R9,R6               R9 = LINE NUMBER                             
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         USING LINED,R6                                                         
         TM    OPTIONS,JOBTOTS                                                  
         BZ    TEND                JUST RUNLAST - NO DISPLAY PENDING            
         L     R0,AIOB                                                          
         ST    R0,DMCB                                                          
         GOTO1 ASETELAD                                                         
         ICM   R4,15,ANAM                                                       
         BZ    T4                                                               
         USING ACNAMED,R4          NAME - ON UP TO 3 LINES                      
         ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'2'                                                         
         GOTO1 VCHOPPER,DMCB,((RF),ACNMNAME),(20,SCANBLCK),3                    
         ICM   RF,15,DMCB+8                                                     
         BZ    T4                                                               
         AR    R9,RF                                                            
         CH    R9,=H'20'                                                        
         BNL   TFULL                                                            
         BCTR  R9,0                                                             
         LR    R8,R6                                                            
         USING LINED,R8                                                         
         LA    R3,SCANBLCK                                                      
T3       OI    LINEHDR+6,X'80'                                                  
         MVC   LINEDATA+13(20),0(R3)                                            
         LA    R3,20(R3)                                                        
         LA    R8,86(R8)                                                        
         BCT   RF,T3                                                            
         DROP  R8                                                               
T4       L     R4,AKEY                                                          
         USING ACKEYD,R4                                                        
         MVC   LINEDATA(12),ACKEYACC+3                                          
         ICM   R5,15,ABAL                                                       
*                                                                               
         L     RE,AIO                                                           
         CLC   1(2,RE),PRODUNIT                                                 
         BNE   T5                                                               
*                                                                               
         BAS   RE,RDOPT                                                         
         BAS   RE,LOOKUP                                                        
*                                                                               
         SPACE 1                                                                
T5       DS    0H                  DISPLAY JOB TOTALS                           
*        ZAP   THISBUCK,ACBLFRWD                                                
         ZAP   THISBUCK,JOBORD                                                  
         LA    R2,THISBUCK                                                      
         LA    R3,LINEDATA+34                                                   
         BAS   RE,EDIT                                                          
*        LA    R2,ACBLURG                                                       
         LA    R2,JOBEST                                                        
         LA    R3,LINEDATA+45                                                   
         BAS   RE,EDIT                                                          
         ZAP   THISBUCK,ACBLDR                                                  
         LA    R2,THISBUCK                                                      
         LA    R3,LINEDATA+56                                                   
         BAS   RE,EDIT                                                          
         ZAP   THISBUCK,ACBLCR                                                  
         LA    R2,THISBUCK                                                      
         LA    R3,LINEDATA+67                                                   
         LA    RE,T6                                                            
EDIT     CP    0(6,R2),=P'0'       EDIT VALUE INTO DISPLAY                      
         BER   RE                                                               
         EDIT  (P6,0(R2)),(11,0(R3)),2,MINUS=YES                                
         BR    RE                                                               
         SPACE 1                                                                
T6       OI    LINEHDR+6,X'80'                                                  
         LA    R9,1(R9)                                                         
         STH   R9,LINE                                                          
         SPACE 1                                                                
T7       AP    GTOTORD,JOBORD      ADD JOB TOTS TO GRAND TOTS                   
         AP    GTOTEST,JOBEST                                                   
         AP    GTOTCHA,ACBLDR                                                   
         AP    GTOTBIL,ACBLCR                                                   
         NI    OPTIONS,X'FF'-JOBTOTS-JOBINUSE                                   
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         CLI   ACKEYACC,RUNLAST                                                 
         BE    TEND                                                             
         ST    R4,DMCB                                                          
         GOTO1 ASETELAD                                                         
         B     T03                 NOW PROCESS THIS JOB REC                     
         SPACE 1                                                                
T8       L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         ZIC   R1,ACKEYACC+L'ACKEYACC-1 REC LIMIT EXCEEDED SO FORCE             
         BCTR  R1,0                     ROOT TO RE-READ AND COP OUT             
         STC   R1,ACKEYACC+L'ACKEYACC-1                                         
         SPACE 1                                                                
TNEXTAC  MVI   KEYMASK+15,X'FF'    ENSURE READ HIGH FOR NEXT A/C                
         MVC   KEYMASK+16(26),KEYMASK+15                                        
         MVC   KEYCHK+15(27),SPACES                                             
         SPACE 1                                                                
TNEXT    LTR   RB,RB                                                            
         B     TXIT                                                             
         SPACE 1                                                                
TEND     CLI   LINE+1,19                                                        
         BE    TFULL                                                            
         LH    R6,LINE                                                          
         MH    R6,=H'86'                                                        
         LA    R6,INFDATAH(R6)                                                  
         MVC   LINEDATA(5),=C'TOTAL'                                            
         LA    R2,GTOTORD                                                       
         LA    R3,LINEDATA+34                                                   
         BAS   RE,EDIT                                                          
         LA    R2,GTOTEST                                                       
         LA    R3,LINEDATA+45                                                   
         BAS   RE,EDIT                                                          
         LA    R2,GTOTCHA                                                       
         LA    R3,LINEDATA+56                                                   
         BAS   RE,EDIT                                                          
         LA    R2,GTOTBIL                                                       
         LA    R3,LINEDATA+67                                                   
         BAS   RE,EDIT                                                          
         OI    LINEHDR+6,X'80'                                                  
         SR    R0,R0                                                            
         B     TXIT                                                             
         SPACE 1                                                                
TFULL    TM    OPTIONS,JOBTOTS     SET CC TO NEG FOR SCREEN FULL                
         BZ    TFULL1                                                           
         L     R4,AIO                                                           
         USING ACCRECD,R4                                                       
         L     RE,AIOB                                                          
         MVC   ACCKEY,0(RE)                                                     
         DROP  R4                                                               
*                                                                               
TFULL1   LNR   RB,RB                                                            
         MVI   LINE+1,3            AND SAVE HEADS                               
TXIT     XIT1                                                                   
         EJECT                                                                  
INITIAL  NTR1                                                                   
*                                                                               
         LA    R2,ROUTTAB          R2=A(ADCON TABLE)                            
         LA    R3,ROUTTABL         R3=LOOP COUNTER                              
         LA    R4,ROUTINES         R4=A(RELOCATED ADDRESSES)                    
*                                                                               
INIT2    L     RF,0(R2)            RELOCATE ADCONS                              
         A     RF,MYRELO                                                        
         ST    RF,0(R4)                                                         
         LA    R2,L'ROUTTAB(R2)                                                 
         LA    R4,L'ROUTINES(R4)                                                
         BCT   R3,INIT2                                                         
*                                                                               
         LA    RF,COLIST                                                        
         ST    RF,ACOLIST                                                       
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'50',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)          GET A(TABLES)                                
*                                                                               
         LM    R0,R1,0(RF)         GET DISP TO/LENGTH OF COLUMN TABLE           
         AR    R0,RF                                                            
         STM   R0,R1,ACOLTAB                                                    
*                                                                               
         LM    R0,R1,8(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,AOPVTAB                                                    
*                                                                               
         GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
         CLI   4(R1),0                                                          
         BNE   INIT4                                                            
         DC    H'0'                                                             
*                                                                               
INIT4    LA    R4,KEYB                                                          
         USING CPYRECD,R4                                                       
         MVC   CPYKEY,SPACES       READ AND BUFFER COMPANY RECORD               
         MVC   CPYKCPY,MYCO                                                     
         GOTO1 AREADB                                                           
         L     R4,AIOB                                                          
         LH    RF,CPYRLEN                                                       
         L     RE,ACOMP            RE=DESTINATION                               
         LR    R0,R4               R0=SOURCE                                    
         LR    R1,RF                                                            
         MVCL  RE,R0               SAVE THE COMPANY RECORD                      
*                                                                               
INITX    B     TXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
RDOPT    NTR1                                                                   
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOABUFF,AOPTBUFF                                                 
         MVC   GOLBUFF,=AL4(L'OPTBUFF)                                          
         MVC   GOACOMP,ACOMP                                                    
         L     R4,AIO              RE-READ THIS RECORD AT END                   
         ST    R4,GOAKEY                                                        
*                                                                               
         L     R4,AIOB             JOB IS SAVED IN IOB                          
         ST    R4,GOAJOB                                                        
*                                                                               
         MVC   GOSELCUL,0(R4)                                                   
         LA    R4,3(R4)                                                         
*                                                                               
         MVC   GOSELCLI,SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,SAVEHIER                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELCLI(0),0(R4)                                                
         LA    R4,1(R1,R4)                                                      
*                                                                               
         MVC   GOSELPRO,SPACES                                                  
         SR    R3,R3                                                            
         IC    R3,SAVEHIER                                                      
         IC    R1,SAVEHIER+2                                                    
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELPRO(0),0(R4)                                                
         LA    R4,1(R1,R4)                                                      
*                                                                               
         MVC   GOSELJOB,SPACES                                                  
         IC    R3,SAVEHIER+2                                                    
         IC    R1,SAVEHIER+4                                                    
         SR    R1,R3                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GOSELJOB(0),0(R4)                                                
*                                                                               
         MVI   GOWHICH,0                                                        
         MVI   GOANYWC,C'N'        DO NOT NEED WORKCODE LEVEL                   
*                                                                               
         GOTO1 VGETOPT,DMCB,GOBLOCK                                             
*                                                                               
RDOPTX   B     TXIT                                                             
         EJECT                                                                  
LOOKUP   NTR1                                                                   
*                                                                               
         ZAP   JOBEST,=P'0'                                                     
         L     R1,AIOB                                                          
         ST    R1,JBAJOB                                                        
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         LA    R1,GOBLOCK                                                       
         ST    R1,JBAGOBLK                                                      
         MVC   JBAIO,AIOC                                                       
         L     R1,AIO                                                           
         ST    R1,JBAKEY                                                        
*                                                                               
         MVC   JBGETOPT,VGETOPT                                                 
*                                                                               
         MVC   JBACOLTB,ACOLTAB                                                 
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
*                                                                               
         GOTO1 VJOBBER,DMCB,JBLOCK                                              
         CLI   JBERROR,X'00'                                                    
         BE    LOOKUPX                                                          
         DC    H'0'                                                             
*                                                                               
LOOKUPX  L     R3,JBACOLTB                                                      
         USING JBCOLD,R3                                                        
         ZAP   JOBEST,JBCOLVAL                                                  
         B     TXIT                                                             
         DROP  R3                                                               
         SPACE 3                                                                
LOOKFLDH DC    AL1(L'LOOKFLD+8),4X'00',AL1(L'LOOKFLD),2X'00'                    
LOOKFLD  DC    C'CE'                                                            
         EJECT                                                                  
*                                                                               
JOBTOTS  EQU   X'40'               JOB DISPLAY PENDING                          
JOBINUSE EQU   X'20'               JOB IN USE - SO OVERRIDE USRLIMIT            
ONLYORDS EQU   X'01'               FILTER JOBS WITH ORDERS                      
*                                                                               
ROUTTAB  DS    0A             TABLE OF ADCONS TO BE RELOCATED                   
         DC    V(ACJOBCOL)                                                      
         DC    A(COMP)                                                          
         DC    A(OPTBUFF)                                                       
ROUTTABL EQU   (*-ROUTTAB)/L'ROUTTAB                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
COMP     DC    2000X'00'                                                        
*                                                                               
OPTBUFF  DS    CL3000                                                           
         ORG   OPTBUFF                                                          
         DC    (L'OPTBUFF)X'00'                                                 
         EJECT                                                                  
LOCALD   DSECT                                                                  
MYRELO   DS    A                                                                
*                                                                               
ROUTINES DS    0A                                                               
VJOBCOL  DS    V                                                                
ACOMP    DS    A                                                                
AOPTBUFF DS    A                                                                
         DS    5A                  SPARE                                        
*                                                                               
THISBUCK DS    PL6                                                              
JOBORD   DS    PL6                                                              
JOBEST   DS    PL6                                                              
*                                                                               
ACOLIST  DS    A                   A(COLUMN LIST)                               
*                                                                               
ACOLTAB  DS    A                   A(COLUMN OUTPUT TABLE)                       
LCOLTAB  DS    F                   L'COLUMN OUTPUT TABLE                        
AOPVTAB  DS    A                   A(OPERAND VALUE TABLE)                       
LOPVTAB  DS    F                   L'OPERAND VALUE TABLE                        
*                                                                               
COLIST   DS    XL200                                                            
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
*                                                                               
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
*ACINQDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACINQDSECT                                                     
         PRINT ON                                                               
         SPACE 2                                                                
GTOTORD  DS    PL6                 SAVED RUNNING TOTALS                         
GTOTEST  DS    PL6                                                              
GTOTCHA  DS    PL6                                                              
GTOTBIL  DS    PL6                                                              
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065ACINQ0E   05/01/02'                                      
         END                                                                    
