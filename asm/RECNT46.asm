*          DATA SET RECNT46    AT LEVEL 031 AS OF 05/01/02                      
*PHASE T80246A,+0                                                               
         TITLE 'T80246 - DX/CX ELECTRONIC CONTRACT DISPLAY/EDIT'                
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT46 (T80246) ---      EC DISP/EDIT                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 29JUL93 (SKU) --- ORIGINAL ROLLOUT                              *             
*                                                                 *             
* 16MAR94 (SKU) --- CATEGORY INPUT LENGTH UP TO 6 CHARS           *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 18APR96 (SKU) --- REFRESH ADV/AGY EQUIVALENCY CODE FOR REP      *             
*                                                                 *             
* 30MAY00 (BU ) --- UPGRADE FOR COLUMBINE USE                     *             
*                                                                 *             
* 29JUN00 (SKU) --- SUPPORT HOME MARKET PROCESSING                *             
*                                                                 *             
*HERE**************************************************************             
*                                                                               
T80246   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80246,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
         L     R2,4(R1)                                                         
                                                                                
         CLC   =C'DISP',0(R2)                                                   
         BNE   DXCX10                                                           
         BAS   RE,DISX                                                          
         B     DXCXXIT                                                          
                                                                                
DXCX10   DS    0H                                                               
         CLC   =C'EDIT',0(R2)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         BAS   RE,CHGX             GO VALIDATE                                  
         BAS   RE,DISX                                                          
                                                                                
DXCXXIT  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*******************************************************************             
* DISPLAY ELECTRONIC CONTRACT INFORMATION                                       
* CONFLICT CODES ARE FROM X'13' ELEMENT IN REGENCON                             
* EOP CODES ARE FROM EOP RECORDS                                                
*******************************************************************             
DISX     NTR1                                                                   
         GOTO1 VFOUTBLK,DMCB,EQCPRDH,EQCLAST,0                                  
                                                                                
         LA    R6,RCONREC          DISPLAY CONFLICT CODES                       
         MVI   ELCODE,X'13'        EC ELEMENT                                   
         BAS   RE,GETEL                                                         
         BNE   DISX40                                                           
                                                                                
         USING RCONCCEL,R6                                                      
         MVC   EQCPRD,RCONCCPR     PRODUCT                                      
         MVC   EQCADV,RCONCCAD     ADVERTISER                                   
                                                                                
         CLI   RCONCCLN,RCONCCL2   CATEGORY CODE IF ANY                         
         BL    *+10                                                             
         MVC   EQCCAT,RCONCCCT                                                  
                                                                                
         MVI   EQCCDY,C'1'         CROSS DAY                                    
                                                                                
         OC    RCONCCDF,RCONCCDF   SKIP IF NO CROSS DAY                         
         BZ    DISX30                                                           
                                                                                
         ZIC   RE,=X'40'           START WITH THE MONDAY BIT ON                 
         LA    RF,C'1'             EDCDIC EQUIVALENT OF MONDAY                  
                                                                                
DISX10   DS    0H                                                               
         STC   RE,WORK                                                          
         CLC   RCONCCDF,WORK                                                    
         BE    DISX20                                                           
         CLI   WORK,0              INCASE OF RUNAWAY LOOP                       
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
                                                                                
         SRL   RE,1                NEXT BIT                                     
         LA    RF,1(RF)            NEXT EBCDIC NUMBER                           
         B     DISX10                                                           
                                                                                
DISX20   DS    0H                                                               
         STC   RF,EQCCDY           CROSS DAY                                    
                                                                                
DISX30   DS    0H                                                               
         MVC   EQCARA,RCONCCAR     AREA                                         
         MVC   EQCOTY,RCONCCOT     ORDER TYPE                                   
         DROP  R6                                                               
                                                                                
DISX40   DS    0H                  DISPLAY EOP CODES                            
*                                                                               
         XC    KEY,KEY             GET STATION TRAFFIC CODE                     
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA  INSERT REP                                   
         MVC   KEY+22(5),RCONKSTA  INSERT STATION                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - STATION RECORD MUST BE FOUND           
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING RSTAREC,R6                                                       
         MVC   TRAFCODE,RSTATRAF   SAVE TRAFFIC CODE                            
*                                                                               
         DROP  R6                                                               
*                                                                               
         GOTO1 GETEOP,DMCB,(X'1B',EQCEADVH),(4,RCONKADV),(15,0)                 
         GOTO1 GETEOP,DMCB,(X'1C',EQCEAGYH),(6,RCONKAGY),(13,1)                 
         GOTO1 GETEOP,DMCB,(X'1D',EQCEOFFH),(2,RCONKOFF),(17,0)                 
         GOTO1 GETEOP,DMCB,(X'1E',EQCESALH),(3,RCONSAL),(16,0)                  
                                                                                
         CLI   TWAACCS,C'$'                                                     
         BE    DISX50                                                           
*                                                                               
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET?                                 
         BO    DISX50                                                           
*                                                                               
         OI    EQCPRDH+6,X'20'     FOR A REP TERMINAL, ALL FIELDS               
         OI    EQCADVH+6,X'20'     EXCEPT CROSS DAY ARE PROTECTED               
         OI    EQCCATH+6,X'20'                                                  
         NI    EQCCDYH+6,X'FF'-X'20'                                            
         OI    EQCARAH+6,X'20'                                                  
         OI    EQCOTYH+6,X'20'                                                  
         OI    EQCEADVH+6,X'20'                                                 
         OI    EQCEAGYH+6,X'20'                                                 
         OI    EQCEOFFH+6,X'20'                                                 
         OI    EQCESALH+6,X'20'                                                 
         B     DISXXIT                                                          
                                                                                
DISX50   DS    0H                                                               
         NI    EQCPRDH+6,X'FF'-X'20' FOR A STATION TERMINAL, ALL FIELDS         
         NI    EQCADVH+6,X'FF'-X'20' EXCEPT CROSS DAY ARE UNPROTECTED           
         NI    EQCCATH+6,X'FF'-X'20'                                            
         OI    EQCCDYH+6,X'20'                                                  
         NI    EQCARAH+6,X'FF'-X'20'                                            
         NI    EQCOTYH+6,X'FF'-X'20'                                            
         NI    EQCEADVH+6,X'FF'-X'20'                                           
         NI    EQCEAGYH+6,X'FF'-X'20'                                           
         NI    EQCEOFFH+6,X'FF'-X'20'                                           
         NI    EQCESALH+6,X'FF'-X'20'                                           
                                                                                
DISXXIT  DS    0H                                                               
         OI    EQCFLGH+4,X'20'     IF CX, WILL CALL CHANGE ROUTINE              
         OI    EQCEADVH+4,X'20'                                                 
         OI    EQCEAGYH+4,X'20'                                                 
         OI    EQCEOFFH+4,X'20'                                                 
         OI    EQCESALH+4,X'20'                                                 
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* CHANGE ELECTRONIC CONTRACT INFORMATION                                        
* CONFLICT CODES ARE FROM X'13' ELEMENT IN REGENCON                             
* EOP CODES ARE FROM EOP RECORDS                                                
*******************************************************************             
CHGX     NTR1                                                                   
         TM    EQCFLGH+4,X'20'     PRE-VAL?                                     
         BZ    CHGXXIT                                                          
*                                                                               
         MVC   KEY+28(4),TWAKADDR  RETRIEVE CONTRACT RECORD                     
         GOTO1 VGETREC,DMCB,RCONREC                                             
*                                                                               
**>>>>>                                                                         
*                                                                               
         XC    KEY,KEY             GET STATION TRAFFIC CODE                     
         MVI   KEY,2                                                            
         MVC   KEY+20(2),REPALPHA  INSERT REP                                   
         MVC   KEY+22(5),RCONKSTA  INSERT STATION                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - STATION RECORD MUST BE FOUND           
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING RSTAREC,R6                                                       
         MVC   TRAFCODE,RSTATRAF   SAVE TRAFFIC CODE                            
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVC   KEY+28(4),TWAKADDR  RETRIEVE CONTRACT RECORD AGAIN               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
**>>>>>                                                                         
                                                                                
         XC    WORK2,WORK2                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHGX10                                                           
                                                                                
         USING RCONCCEL,R6                                                      
         CLI   TWAACCS,C'$'        FOR REP TERMINALS, ONLY CROSS DAY            
         BE    CHGX05              GETS CHANGED.  SO WE'RE KEEPING THE          
*                                  REST                                         
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET?                                 
         BO    CHGX05                                                           
*                                                                               
         MVC   WORK2(RCONCCL2),RCONCCEL                                         
                                                                                
CHGX05   DS    0H                                                               
         MVC   WORK2+8(2),RCONCCCD SAVE CROSSDAY AND CROSSDAY DEFAULT           
*                                  FOR STATION                                  
         GOTO1 VDELELEM,DMCB,(X'13',RCONREC)                                    
         DROP  R6                                                               
                                                                                
CHGX10   DS    0H                                                               
         LA    R3,2                INVALID INPUT                                
                                                                                
         LA    R6,WORK2                                                         
         USING RCONCCEL,R6                                                      
         MVI   RCONCCCO,X'13'                                                   
                                                                                
         CLI   TWAACCS,C'$'        IF REP TERMINAL, EDIT ONLY CROSS DAY         
         BE    CHGX50                                                           
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET?                                 
         BO    CHGX50                                                           
*                                                                               
                                                                                
* FOR A REP TERMINAL                                                            
                                                                                
         LA    R2,EQCCDYH          CROSS DAY                                    
         MVI   RCONCCDF,X'40'      DEFAULT MONDAY                               
         CLI   5(R2),0             MUST BE 1-7 (MONDAY - SUNDAY)                
         BE    CHGX40                                                           
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
                                                                                
CHGX20   DS    0H                                                               
         CLI   8(R2),C'1'                                                       
         BL    ERROR                                                            
         CLI   8(R2),C'7'                                                       
         BH    ERROR                                                            
                                                                                
         ZIC   RE,=X'40'           START WITH THE MONDAY BIT ON                 
         LA    RF,C'1'             EDCDIC EQUIVALENT OF MONDAY                  
                                                                                
CHGX25   DS    0H                                                               
         STC   RF,WORK                                                          
         CLC   WORK(1),8(R2)                                                    
         BE    CHGX30                                                           
         CLI   WORK,C'7'           INCASE OF RUNAWAY LOOP                       
         BNH   *+6                                                              
         DC    H'0'                SHOULD NEVER HAPPEN                          
                                                                                
         SRL   RE,1                NEXT BIT                                     
         LA    RF,1(RF)            NEXT EBCDIC NUMBER                           
         B     CHGX25                                                           
                                                                                
CHGX30   DS    0H                                                               
         STC   RE,RCONCCDF         CROSS DAY DEFAULT                            
         MVC   WORK(1),RCONCCDF                                                 
         NC    WORK(1),RCONCCCD                                                 
         BZ    CHGX40                                                           
         LA    R3,400              CROSS DAY IS ALREADY CLOSED                  
         B     ERROR                                                            
                                                                                
CHGX40   DS    0H                                                               
         OC    RCONCCOT,RCONCCOT   IF NEW ELEMENT                               
         BNZ   CHGX45                                                           
         MVI   RCONCCOT,C'5'       SET ORDER TYPE DEFAULT TO 5                  
                                                                                
CHGX45   DS    0H                                                               
         OC    RCONCCLN,RCONCCLN   NO LENGTH MEANS THE ELEMENT IS NEW           
         BNZ   CHGX48                                                           
         MVI   RCONCCLN,RCONCCL1   SO USE DEFAULT LENGTH                        
                                                                                
CHGX48   DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
*        GOTO1 VPUTREC,DMCB,RCONREC                                             
*        B     CHGXXIT                                                          
         B     CHGX130             REFRESH CODES, MIGHT BE CHGED IN SFM         
         EJECT                                                                  
* FOR A STATION TERMINAL                                                        
                                                                                
CHGX50   DS    0H                  FOR STA, EDIT ALL BUT CROSS DAY              
         LA    R2,EQCPRDH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    CHGX60                                                           
         CLI   5(R2),3             MUST BE 3 CHARACTERS                         
         BNE   ERROR                                                            
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
         MVC   RCONCCPR,8(R2)                                                   
                                                                                
CHGX60   DS    0H                                                               
         LA    R2,EQCADVH          ADVERTISER                                   
         CLI   5(R2),0                                                          
         BE    CHGX70                                                           
         CLI   5(R2),3                                                          
         BNE   ERROR                                                            
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
         MVC   RCONCCAD,8(R2)                                                   
                                                                                
CHGX70   DS    0H                                                               
         LA    R2,EQCCATH          CATEGORY                                     
         MVI   RCONCCLN,RCONCCL1   LENGTH IF NO CATEGORY                        
         CLI   5(R2),0                                                          
         BE    CHGX80                                                           
         CLI   5(R2),6             UP TO 6 CHARS                                
         BH    ERROR                                                            
         MVI   RCONCCLN,RCONCCL2   LENGTH IF CATEGORY                           
         ZIC   R1,0(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RCONCCCT(0),8(R2)                                                
         OC    RCONCCCT,MYSPACES   BLANK PAD                                    
                                                                                
CHGX80   DS    0H                                                               
         LA    R2,EQCARAH          AREA                                         
         CLI   5(R2),0                                                          
         BE    CHGX100                                                          
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         CLI   8(R2),C'N'          MUST BE N, R, L OR O                         
         BE    CHGX90                                                           
         CLI   8(R2),C'R'                                                       
         BE    CHGX90                                                           
         CLI   8(R2),C'L'                                                       
         BE    CHGX90                                                           
         CLI   8(R2),C'O'                                                       
         BNE   ERROR                                                            
CHGX90   DS    0H                                                               
         MVC   RCONCCAR,8(R2)                                                   
                                                                                
CHGX100  DS    0H                                                               
         LA    R2,EQCOTYH          ORDER TYPE                                   
         CLI   5(R2),0                                                          
         BNE   CHGX110                                                          
         MVI   RCONCCOT,C'5'       DEFAULT IS 5                                 
         B     CHGX120                                                          
                                                                                
CHGX110  DS    0H                                                               
         CLI   8(R2),C'1'          MUST BE 1-9                                  
         BL    ERROR                                                            
         CLI   8(R2),C'9'                                                       
         BH    ERROR                                                            
         MVC   RCONCCOT,8(R2)                                                   
                                                                                
CHGX120  DS    0H                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
                                                                                
CHGX130  DS    0H                                                               
         LA    R6,RCONREC          STORE STA ADV/AGY CODE IN ORD                
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHGX150                                                          
         USING RCONXXEL,R6                                                      
         XC    RCONXADV,RCONXADV                                                
         XC    RCONXAGY,RCONXAGY                                                
         MVC   RCONXADV(L'EQCEADV),EQCEADV                                      
         MVC   RCONXAGY(L'EQCEAGY),EQCEAGY                                      
         B     CHGX170                                                          
         DROP  R6                                                               
                                                                                
CHGX150  DS    0H                                                               
         OC    EQCEADV,EQCEADV                                                  
         BNZ   CHGX160                                                          
         OC    EQCEAGY,EQCEAGY                                                  
         BZ    CHGX170                                                          
                                                                                
CHGX160  DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RCONXXEL,R6                                                      
         MVI   RCONXXEL,X'9F'                                                   
         MVI   RCONXXEL+1,80                                                    
         MVC   RCONXADV(L'EQCEADV),EQCEADV                                      
         MVC   RCONXAGY(L'EQCEAGY),EQCEAGY                                      
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         DROP  R6                                                               
                                                                                
CHGX170  DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
                                                                                
         LA    R2,EQCEADVH                                                      
         TM    6(R2),X'20'         SKIP IF PROTECTED                            
         BO    CHGX180                                                          
         TM    4(R2),X'20'         SKIP IF NOT CHANGED                          
         BO    CHGX180                                                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
                                                                                
         GOTO1 CHGEOP,DMCB,(X'1B',EQCEADVH),(4,RCONKADV),(15,0)                 
                                                                                
CHGX180  DS    0H                                                               
                                                                                
         LA    R2,EQCEAGYH                                                      
         TM    6(R2),X'20'         SKIP IF PROTECTED                            
         BO    CHGX190                                                          
         TM    4(R2),X'20'         SKIP IF NOT CHANGED                          
         BO    CHGX190                                                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
                                                                                
         GOTO1 CHGEOP,DMCB,(X'1C',EQCEAGYH),(6,RCONKAGY),(13,1)                 
                                                                                
CHGX190  DS    0H                                                               
                                                                                
         LA    R2,EQCEOFFH                                                      
         TM    6(R2),X'20'         SKIP IF PROTECTED                            
         BO    CHGX200                                                          
         TM    4(R2),X'20'         SKIP IF NOT CHANGED                          
         BO    CHGX200                                                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
                                                                                
         GOTO1 CHGEOP,DMCB,(X'1D',EQCEOFFH),(2,RCONKOFF),(17,0)                 
                                                                                
CHGX200  DS    0H                                                               
                                                                                
         LA    R2,EQCESALH                                                      
         TM    6(R2),X'20'         SKIP IF PROTECTED                            
         BO    CHGXXIT                                                          
         TM    4(R2),X'20'         SKIP IF NOT CHANGED                          
         BO    CHGXXIT                                                          
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
                                                                                
         GOTO1 CHGEOP,DMCB,(X'1E',EQCESALH),(3,RCONSAL),(16,0)                  
                                                                                
CHGXXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* RETRIEVE CORRESPONDING EOP RECORD FOR EOP DISPLAY                             
* WILL ALSO PROTECT FIELD IF ACTIVE FLAG IS SET                                 
* P1=(B1)RECORD TYPE, (B2-3)SCREEN HEADER ADDRESS FOR DISPLAY                   
* P2=(B1)LENGTH OF EOP FIELD, (B2-3) ADDRESS OF EOP FIELD                       
* P3=(B1)OFFSET TO FIRST NON-NULL FIELD OF EOP KEY                              
*******************************************************************             
GETEOP   NTR1                                                                   
         MVC   WORK(12),0(R1)                                                   
         XC    HALF,HALF           CLEAR PASS-BACK FIELD                        
*                                                                               
*   PASS-BACK FIELD IS USED FOR TRADE AGENCY PROCESSING.  THIS WILL             
*        CONTAIN THE ALTERNATE TRADE OFFICE CODE IF A TRADE                     
*        ORDER'S AGENCY CODE MUST BE ASSIGNED.                                  
*        DO NOT - REPEAT, NOT - STEP ON THE FIRST BYTE OF HALF                  
*        IN THIS ROUTINE.                                                       
*                                                                               
         CLI   WORK+11,0           P3 (2-4) ZERO? (NOT AGENCY)                  
         BE    GETE0016            YES - NOT AGENCY                             
         LA    R6,RCONREC          DISPLAY CONFLICT CODES                       
         MVI   ELCODE,X'1E'        RANDOM FLAG ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   GETE0016            NOT FOUND                                    
         TM    RCONRF1-RCONRFEL(R6),X'08'                                       
*                                  TRADE ORDER SET?                             
         BNO   GETE0016            NO                                           
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'1A'           SET AGENCY 2 REC                             
         MVC   KEY+19(6),RCONKAGY  INSERT AGENCY+OFF                            
         MVC   KEY+25(2),RCONKREP  INSERT REP CODE                              
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                KEY MUST BE ON FILE                          
         GOTO1 VGETREC,DMCB,IOAREA RETRIEVE THE AGENCY2 RECORD                  
         LA    R6,IOAREA                                                        
         USING RAGY2REC,R6                                                      
         LA    RF,RAGY2FXE         1ST ELEMENT IN RAGY2REC                      
GETE0010 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO  -                                        
         DC    H'0'                MUST FIND THE ELEMENT                        
         CLI   0(RF),X'1F'         AGENCY ELEMENT?                              
         BE    GETE0015            YES -                                        
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     GETE0010            GO BACK FOR NEXT                             
GETE0015 EQU   *                                                                
         MVI   HALF,X'FF'          SET NO-FIND FOR ALT TRADE                    
         CLI   RAG2TRAD-RAG2ELEM(RF),C' '                                       
*                                  ANYTHING IN ALT TRADE CODE FIELD?            
         BNH   GETE0016            NO  - PASS BACK EMPTY FIELD                  
         MVC   HALF(1),RAG2TRAD-RAG2ELEM(RF)                                    
*                                  PASS BACK ALT TRADE CODE                     
GETE0016 EQU   *                                                                
                                                                                
                                                                                
         LA    R6,KEY                                                           
         USING REOPKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVC   REOPKTYP(1),WORK    RECORD TYPE                                  
         DROP  R6                                                               
                                                                                
         ZIC   RF,WORK+8           GET OFFSET OF EOP FIELDS IN KEY              
         AR    R6,RF                                                            
         MVC   0(L'REOPKREP,R6),REPALPHA   REP                                  
         MVI   2(R6),1                     BIAS                                 
         CLI   TRAFCODE,C'B'       BIAS STATION?                                
         BE    GETE0020            YES                                          
         CLI   TRAFCODE,C'W'       BIAS STATION?                                
         BE    GETE0020            YES                                          
         MVI   2(R6),2                                                          
         CLI   TRAFCODE,C'J'       JDS  STATION?                                
         BE    GETE0020            YES                                          
         MVI   2(R6),3                                                          
         CLI   TRAFCODE,C'K'       ESG  STATION?                                
         BE    GETE0020            YES                                          
         MVI   2(R6),4                                                          
         CLI   TRAFCODE,C'C'       COLUMBINE STATION?                           
         BE    GETE0020            YES                                          
         DC    H'0'                UNRECOGNIZED TRAFFIC SYSTEM                  
GETE0020 EQU   *                                                                
         MVC   3(L'REOPKSTA,R6),RCONKSTA   STATION+MEDIA                        
                                                                                
         ZICM  RF,WORK+5,3                                                      
         ZIC   R1,WORK+4                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),0(RF)       ADVERTISER/AGENCY/OFFICE/SALESPERSON         
*                                                                               
*   VALUE IN 'HALF' IS SET LOCALLY, IF THE AGENCY2 RECORD CONTAINS              
*        AN ALTERNATE AGENCY CODE VALUE.  THE AGENCY OFFICE IS THEN             
*        OVERLAID BY THE SPECIAL CHARACTER + ALTERNATE AGENCY CODE              
*        PRIOR TO ACCESSING THE RECORD.                                         
*                                                                               
         CLI   HALF,C' '           ANY TRADE ALT AGENCY CODE?                   
         BNH   GETE0025            NO                                           
         MVI   12(R6),C'#'         YES - INSERT INDICATOR                       
         MVC   13(1,R6),HALF       INSERT TRADE ALT AGENCY CODE                 
GETE0025 EQU   *                                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
         BNE   GETEOPX                                                          
                                                                                
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING REOPREC,R6                                                       
         ZICM  RF,WORK+1,3                                                      
         MVC   8(6,RF),REOPEQUV                                                 
                                                                                
         TM    REOPFLAG,X'80'      IF CODE ACTIVE, PROTECT FIELD                
         BZ    GETEOPX                                                          
         OI    1(RF),X'20'                                                      
                                                                                
GETEOPX  DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* RETRIEVE CORRESPONDING EOP RECORD FOR EOP CHANGE                              
* P1=(B1)RECORD TYPE, (B2-3)SCREEN HEADER ADDRESS WITH DATA                     
* P2=(B1)LENGTH OF EOP FIELD, (B2-3) ADDRESS OF EOP FIELD                       
* P3=(B1)OFFSET TO FIRST NON-NULL FIELD OF EOP KEY                              
* P3=(B2-3)NON-ZERO:  SPECIAL AGENCY/TRADE PROCESSING                           
*******************************************************************             
CHGEOP   NTR1                                                                   
         MVC   WORK(12),0(R1)                                                   
         XC    HALF,HALF           CLEAR PASS-BACK FIELD                        
*                                                                               
*   PASS-BACK FIELD IS USED FOR TRADE AGENCY PROCESSING.  THIS WILL             
*        CONTAIN THE ALTERNATE TRADE OFFICE CODE IF A TRADE                     
*        ORDER'S AGENCY CODE MUST BE ASSIGNED.                                  
*        DO NOT - REPEAT, NOT - STEP ON THE FIRST BYTE OF HALF                  
*        IN THIS ROUTINE.                                                       
*                                                                               
         CLI   WORK+11,0           P3 (2-4) ZERO? (NOT AGENCY)                  
         BE    CHGE0016            YES - NOT AGENCY                             
         LA    R6,RCONREC          DISPLAY CONFLICT CODES                       
         MVI   ELCODE,X'1E'        RANDOM FLAG ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   CHGE0016            NOT FOUND                                    
         TM    RCONRF1-RCONRFEL(R6),X'08'                                       
*                                  TRADE ORDER SET?                             
         BNO   CHGE0016            NO                                           
         GOTO1 =A(RETRADE),DMCB,HALF,RCONREC,IOAREA,RR=Y                        
CHGE0016 EQU   *                                                                
         LA    R6,IOAREA                                                        
         USING REOPKEY,R6                                                       
         XC    IOAREA(256),IOAREA                                               
         MVC   REOPKTYP(1),WORK    RECORD TYPE                                  
         DROP  R6                                                               
                                                                                
         ZIC   RF,WORK+8           GET OFFSET OF EOP FIELDS IN KEY              
         AR    R6,RF                                                            
         MVC   0(L'REOPKREP,R6),REPALPHA   REP                                  
         MVI   2(R6),1                     BIAS                                 
         CLI   TRAFCODE,C'B'       BIAS STATION?                                
         BE    CHGE0020            YES                                          
         CLI   TRAFCODE,C'W'       BIAS STATION?                                
         BE    CHGE0020            YES                                          
         MVI   2(R6),2                                                          
         CLI   TRAFCODE,C'J'       JDS  STATION?                                
         BE    CHGE0020            YES                                          
         MVI   2(R6),3                                                          
         CLI   TRAFCODE,C'K'       ESG  STATION?                                
         BE    CHGE0020            YES                                          
         MVI   2(R6),4                                                          
         CLI   TRAFCODE,C'C'       COLUMBINE STATION?                           
         BE    CHGE0020            YES                                          
         DC    H'0'                UNRECOGNIZED TRAFFIC SYSTEM                  
CHGE0020 EQU   *                                                                
         MVC   3(L'REOPKSTA,R6),RCONKSTA   STATION+MEDIA                        
                                                                                
         ZICM  RF,WORK+5,3                                                      
         ZIC   R1,WORK+4                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),0(RF)       ADVERTISER/AGENCY/OFFICE/SALESPERSON         
         CLI   HALF,C' '           ANY TRADE AGENCY OFFICE?                     
         BNH   CHGE0030            NO  - CAN ONLY BE SET LOCALLY                
*                                     BY THE RETRADE ROUTINE                    
         MVI   12(R6),C'#'         YES - INSERT SPECIAL INDICATOR               
         MVC   13(1,R6),HALF       INSERT TRADE OFFICE CODE                     
*                                                                               
CHGE0030 EQU   *                                                                
                                                                                
         OI    DMINBTS,X'08'       READ DELETES                                 
         MVC   KEY(L'REOPKEY),IOAREA                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
         BE    CHGEOP50                                                         
                                                                                
         LA    R6,IOAREA                                                        
         USING REOPKEY,R6                                                       
         MVI   REOPLEN+1,50        REC LEN (34 + 16)                            
         MVI   REOPCODE,X'01'      EOP ELEMENT CODE                             
         MVI   REOPELLN,16         EOP ELEMENT LENGTH                           
         GOTO1 DATCON,DMCB,(5,0),(3,REOPDATE)                                   
         CLI   WORK+11,0           P3 (2-4) ZERO? (NOT AGENCY)                  
         BE    CHGE0040            YES - NOT AGENCY                             
         MVC   REOPTROF,RCONKAOF   INSERT AGENCY OFFICE                         
CHGE0040 EQU   *                                                                
         ZICM  R3,WORK+1,3                                                      
         XC    REOPEQUV,REOPEQUV                                                
         MVC   REOPEQUV(6),=6C'0'     LEADING ZEROES                            
         LA    RE,REOPEQUV+6                                                    
         ZIC   RF,5(R3)                                                         
         SR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R3)                                                    
         DROP  R6                                                               
                                                                                
         DS    0H                                                               
         GOTO1 VADDREC,DMCB,IOAREA ADD THE RECORD                               
         B     CHGEOPX                                                          
                                                                                
CHGEOP50 DS    0H                  CHANGE EXISTING RECORD                       
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING REOPREC,R6                                                       
                                                                                
         TM    REOPFLAG,X'80'      CODE ACTIVE IN ORDER                         
         BZ    CHGEOP55                                                         
         LA    R3,406              ERROR, CANNOT CHANGE AN ACTIVE CODE          
         B     ERROR                                                            
                                                                                
CHGEOP55 DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,REOPDATE)                                   
         ZICM  R3,WORK+1,3                                                      
                                                                                
         XC    REOPEQUV,REOPEQUV                                                
         MVC   REOPEQUV(6),=6C'0'     LEADING ZEROES                            
         LA    RE,REOPEQUV+6                                                    
         ZIC   RF,5(R3)                                                         
         SR    RE,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R3)                                                    
                                                                                
         NI    REOPCNTL,X'FF'-X'80'  RESTORE INCASE RECORD IS DELETED           
                                                                                
         GOTO1 VPUTREC,DMCB,IOAREA                                              
                                                                                
         TM    KEY+27,X'80'        RESTORE DELETED RECORD                       
         BZ    CHGEOPX                                                          
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE                                                           
                                                                                
CHGEOPX  DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
XIT      XIT1                                                                   
*                                                                               
TRAFCODE DS    CL1                 STATION TRAFFIC CODE                         
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONLAST                                                          
       ++INCLUDE RECNTE8D                                                       
         CSECT                                                                  
       ++INCLUDE RETRADE                                                        
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031RECNT46   05/01/02'                                      
         END                                                                    
