*          DATA SET RECNT47    AT LEVEL 016 AS OF 05/01/02                      
*PHASE T80247A,+0                                                               
         TITLE 'T80247 - JDS/2000 ELECTRONIC CONTRACT DISPLAY/EDIT'             
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT47 (T80247) --- JDS/2000 EC DISP/EDIT               *             
*                             USED BY COLUMBINE AS WELL AS        *             
*                             JDS/2000.  SOFTENED TO PERMIT USE   *             
*                             BY ANY TRAFFIC SYSTEM, IF CALLED    *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 26AUG93 (SKU) --- ORIGINAL ROLLOUT                              *             
*                                                                 *             
* 12OCT94 (BU ) --- CHANGE CONTRACT SEPARATION FIELD VALIDATION   *             
*                   AND DISPLAY.                                  *             
*         (SKU) --- SET EOP FIELD PROTECTED IF ACTIVE             *             
*                                                                 *             
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                           *             
*                                                                 *             
* 08SEP00 (BU ) --- ACCESS EOP RECORDS WITH ALL TRAFFIC CODES     *             
*                   VIA TABLE LOOKUP                              *             
*                                                                 *             
*HERE**************************************************************             
*                                                                               
T80247   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80247,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
         L     R2,4(R1)                                                         
                                                                                
         BAS   RE,PROFILE                                                       
         BAS   RE,STATION          DETERMINE TRAFFIC CODE                       
                                                                                
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
* CALL TO GETPROF TO RETRIEVE PROFILE INFORMATION                               
*******************************************************************             
PROFILE  NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'R0EC'                                                 
         CLI   TWAACCS,C'$'                                                     
         BE    PROF10                                                           
         MVC   WORK+4(2),REPALPHA  IF REP TERMINAL                              
         B     PROF20                                                           
                                                                                
PROF10   DS    0H                                                               
         MVC   WORK+4(2),TWAUSRID  IF STATION TERMINAL                          
                                                                                
PROF20   DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* DISPLAY ELECTRONIC CONTRACT INFORMATION                                       
* EOP CODES ARE FROM EOP RECORDS                                                
* CONFLICT CODES ARE FROM X'13' ELEMENT IN REGENCON                             
*******************************************************************             
DISX     NTR1                                                                   
         GOTO1 VFOUTBLK,DMCB,EQCEADVH,EQCLAST,0                                 
                                                                                
         CLI   TRAFCODE,2          JDS/2000 PROCESSING?                         
         BNE   DISX10              NO                                           
         GOTO1 GETEOP,DMCB,(X'1B',EQCEADVH),(4,RCONKADV),(15,4),0               
         GOTO1 GETEOP,DMCB,(X'1C',EQCEAGYH),(6,RCONKAGY),(13,6),1               
         GOTO1 GETEOP,DMCB,(X'1D',EQCEOFFH),(2,RCONKOFF),(17,3),0               
         GOTO1 GETEOP,DMCB,(X'1E',EQCESALH),(3,RCONSAL),(16,4),0                
         B     DISX28                                                           
DISX10   EQU   *                                                                
         CLI   TRAFCODE,4          COLUMBINE PROCESSING?                        
         BNE   DISX12              NO                                           
         GOTO1 GETEOP,DMCB,(X'1B',EQCEADVH),(4,RCONKADV),(15,5),0               
         GOTO1 GETEOP,DMCB,(X'1C',EQCEAGYH),(6,RCONKAGY),(13,5),1               
         GOTO1 GETEOP,DMCB,(X'1D',EQCEOFFH),(2,RCONKOFF),(17,3),0               
         GOTO1 GETEOP,DMCB,(X'1E',EQCESALH),(3,RCONSAL),(16,3),0                
         B     DISX28                                                           
DISX12   EQU   *                                                                
         DC    H'0'                UNRECOGNIZED TRAFFIC CODE                    
DISX28   EQU   *                                                                
         LA    R6,RCONREC          DISPLAY CONFLICT CODES                       
         MVI   ELCODE,X'13'        EC ELEMENT                                   
         BAS   RE,GETEL                                                         
         BNE   DISX35                                                           
                                                                                
         LA    R3,JDSDXCX                                                       
                                                                                
DISX30   DS    0H                  DISPLAY ALL FIELDS                           
         L     RF,0(R3)                                                         
         RELOC                                                                  
         AR    RF,RE                                                            
         BASR  RE,RF               GOTO ROUTINE                                 
         LA    R3,L'JDSDXCX(R3)                                                 
         CLI   0(R3),X'FF'                                                      
         BNE   DISX30                                                           
                                                                                
DISX35   DS    0H                  DISPLAY ALL FIELDS                           
         LA    R3,SVPROF                                                        
         LA    R4,15                                                            
         LA    R2,EQCRATEH                                                      
                                                                                
DISX40   DS    0H                  CHECK PROFILE TO PROTECT FIELDS              
         NI    6(R2),X'FF'-X'20'   DEFAULT TO UNPROTECTED                       
         CLI   0(R3),C'N'                                                       
         BNE   DISX50                                                           
         OI    6(R2),X'20'         MAKE PROTECTED                               
                                                                                
DISX50   DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         LA    R3,1(R3)                                                         
         BCT   R4,DISX40                                                        
                                                                                
DISXXIT  DS    0H                                                               
         OI    EQCEADVH+4,X'20'                                                 
         OI    EQCEAGYH+4,X'20'                                                 
         OI    EQCEOFFH+4,X'20'                                                 
         OI    EQCESALH+4,X'20'                                                 
         OI    EQCFLGH+4,X'20'     IF CX, WILL CALL CHANGE ROUTINE              
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* CHANGE ELECTRONIC CONTRACT INFORMATION                                        
* EOP CODES ARE FROM EOP RECORDS                                                
* CONFLICT CODES ARE FROM X'13' ELEMENT IN REGENCON                             
*******************************************************************             
CHGX     NTR1                                                                   
         TM    EQCFLGH+4,X'20'     PRE-VAL?                                     
         BZ    CHGXXIT                                                          
                                                                                
         LA    R3,INVINP                                                        
* VALIDATE EOP FIELDS                                                           
                                                                                
         LA    R2,EQCEADVH                                                      
         TM    6(R2),X'20'         SKIP IF PROTECTED                            
         BO    CHGX10                                                           
         TM    4(R2),X'20'         SKIP IF NOT CHANGED                          
         BO    CHGX10                                                           
                                                                                
         CLI   TRAFCODE,2          JDS/2000?                                    
         BNE   CHGX04              NO                                           
         GOTO1 CHGEOP,DMCB,(X'1B',EQCEADVH),(4,RCONKADV),(15,4),0               
         B     CHGX10                                                           
CHGX04   EQU   *                                                                
         CLI   TRAFCODE,4          COLUMBINE?                                   
         BNE   CHGX06              NO                                           
         GOTO1 CHGEOP,DMCB,(X'1B',EQCEADVH),(4,RCONKADV),(15,5),0               
         B     CHGX10                                                           
CHGX06   EQU   *                                                                
         DC    H'0'                UNRECOGNIZED TRAFFIC CODE                    
                                                                                
CHGX10   DS    0H                                                               
                                                                                
         LA    R2,EQCEAGYH                                                      
         TM    6(R2),X'20'         SKIP IF PROTECTED                            
         BO    CHGX20                                                           
         TM    4(R2),X'20'         SKIP IF NOT CHANGED                          
         BO    CHGX20                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
                                                                                
         CLI   TRAFCODE,2          JDS/2000?                                    
         BNE   CHGX14              NO                                           
         GOTO1 CHGEOP,DMCB,(X'1C',EQCEAGYH),(6,RCONKAGY),(13,6),1               
         B     CHGX20                                                           
CHGX14   EQU   *                                                                
         CLI   TRAFCODE,4          COLUBMINE?                                   
         BNE   CHGX16              NO                                           
         GOTO1 CHGEOP,DMCB,(X'1C',EQCEAGYH),(6,RCONKAGY),(13,5),1               
         B     CHGX20                                                           
CHGX16   EQU   *                                                                
         DC    H'0'                UNRECOGNIZED TRAFFIC CODE                    
CHGX20   DS    0H                                                               
                                                                                
         LA    R2,EQCEOFFH                                                      
         TM    6(R2),X'20'         SKIP IF PROTECTED                            
         BO    CHGX30                                                           
         TM    4(R2),X'20'         SKIP IF NOT CHANGED                          
         BO    CHGX30                                                           
                                                                                
         CLI   TRAFCODE,2          JDS/2000?                                    
         BNE   CHGX24                                                           
         GOTO1 CHGEOP,DMCB,(X'1D',EQCEOFFH),(2,RCONKOFF),(17,3),0               
         B     CHGX30                                                           
CHGX24   EQU   *                                                                
         CLI   TRAFCODE,4          COLUMBINE?                                   
         BE    CHGX30              COLUMBINE DOESN'T USE OFFICE CODE            
CHGX26   DC    H'0'                UNRECOGNIZED TRAFFIC CODE                    
                                                                                
CHGX30   DS    0H                                                               
                                                                                
         LA    R2,EQCESALH                                                      
         TM    6(R2),X'20'         SKIP IF PROTECTED                            
         BO    CHGX40                                                           
         TM    4(R2),X'20'         SKIP IF NOT CHANGED                          
         BO    CHGX40                                                           
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    ERROR                                                            
                                                                                
         CLI   TRAFCODE,2          JDS/2000?                                    
         BNE   CHGX34                                                           
         GOTO1 CHGEOP,DMCB,(X'1E',EQCESALH),(3,RCONSAL),(16,4),0                
         B     CHGX40                                                           
CHGX34   EQU   *                                                                
         CLI   TRAFCODE,4          COLUMBINE?                                   
         BNE   CHGX36              NO                                           
         GOTO1 CHGEOP,DMCB,(X'1E',EQCESALH),(3,RCONSAL),(16,3),0                
         B     CHGX40                                                           
CHGX36   EQU   *                                                                
         DC    H'0'                                                             
         EJECT                                                                  
CHGX40   DS    0H                                                               
         LA    R5,JDSDXCX                                                       
         LA    R4,SVPROF                                                        
         LA    R3,INVINP           INVALID INPUT                                
                                                                                
CHGX50   DS    0H                  VALIDATE REST OF FIELDS                      
         CLI   0(R4),C'N'          AS SPECIFIED BY PROFILE                      
         BE    CHGX60                                                           
         L     RF,4(R5)                                                         
         RELOC                                                                  
         AR    RF,RE                                                            
         BASR  RE,RF                                                            
                                                                                
CHGX60   DS    0H                                                               
         LA    R4,1(R4)                                                         
         LA    R5,L'JDSDXCX(R5)                                                 
         CLI   0(R5),X'FF'                                                      
         BNE   CHGX50                                                           
                                                                                
         MVC   KEY+28(4),TWAKADDR                                               
         MVI   UPDATE,C'Y'         SET FOR UPDATE                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
                                                                                
         XC    WORK2,WORK2                                                      
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHGX100                                                          
         GOTO1 VDELELEM,DMCB,(X'13',RCONREC)                                    
                                                                                
CHGX100  DS    0H                                                               
         LA    R6,WORK2                                                         
         USING RCONCJEL,R6                                                      
                                                                                
         MVI   RCONCJCO,X'13'                                                   
         MVI   RCONCJLN,30                                                      
                                                                                
         MVC   RCONCJRC,EQCRATE                                                 
                                                                                
         LA    R2,EQCCUSH                                                       
         CLI   5(R2),0                                                          
         BE    CHGX110                                                          
         GOTO1 VPACK                                                            
         STCM  R0,7,RCONCJBC                                                    
                                                                                
CHGX110  DS    0H                                                               
         MVC   RCONCJCY,EQCCYC                                                  
         MVC   RCONCJRV,EQCRCAT                                                 
         MVC   RCONCJTF,EQCTFLG                                                 
         MVC   RCONCJP1,EQCPCT1                                                 
         MVC   RCONCJP2,EQCPCT2                                                 
         MVC   RCONCJCT,EQCCTYP                                                 
         MVC   RCONCJCS,CONTSEP                                                 
         MVC   RCONCJSH,EQCSHAN                                                 
         MVC   RCONCJIC,EQCICMT                                                 
         MVC   RCONCJIM,EQCIMSC                                                 
         MVC   RCONCJST,EQCSTAX                                                 
         MVC   RCONCJNR,EQCNRL                                                  
         MVC   RCONCJTO,EQCTORD                                                 
         DROP  R6                                                               
                                                                                
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
                                                                                
         LA    R6,RCONREC          STORE STA ADV/AGY CODE IN ORD                
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHGX140                                                          
         USING RCONXXEL,R6                                                      
         XC    RCONXADV,RCONXADV                                                
         XC    RCONXAGY,RCONXAGY                                                
         MVC   RCONXADV(L'EQCEADV),EQCEADV                                      
         MVC   RCONXAGY(L'EQCEAGY),EQCEAGY                                      
         B     CHGX160                                                          
         DROP  R6                                                               
                                                                                
CHGX140  DS    0H                                                               
         OC    EQCEADV,EQCEADV                                                  
         BNZ   CHGX150                                                          
         OC    EQCEAGY,EQCEAGY                                                  
         BZ    CHGX160                                                          
                                                                                
CHGX150  DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RCONXXEL,R6                                                      
         MVI   RCONXXEL,X'9F'                                                   
         MVI   RCONXXEL+1,80                                                    
         MVC   RCONXADV(L'EQCEADV),EQCEADV                                      
         MVC   RCONXAGY(L'EQCEAGY),EQCEAGY                                      
         GOTO1 VADDELEM,DMCB,RCONREC,WORK2                                      
         DROP  R6                                                               
                                                                                
CHGX160  DS    0H                                                               
         GOTO1 VPUTREC,DMCB,RCONREC                                             
                                                                                
CHGXXIT  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* DISPLAY ROUTINES                                                              
*******************************************************************             
         USING RCONCJEL,R6                                                      
DXRATE   DS    0H                  RATE                                         
         MVC   EQCRATE(L'RCONCJRC),RCONCJRC                                     
         BR    RE                                                               
                                                                                
DXBCUS   DS    0H                  BILLING CUSTOMER                             
         LR    R4,RE                                                            
         EDIT  RCONCJBC,(6,EQCCUS),ALIGN=LEFT                                   
         BR    R4                                                               
                                                                                
DXBCYC   DS    0H                  BILLING CYCLE                                
         MVC   EQCCYC(L'RCONCJCY),RCONCJCY                                      
         BR    RE                                                               
                                                                                
DXRCAT   DS    0H                  REVENUE CATEGORY                             
         MVC   EQCRCAT(L'RCONCJRV),RCONCJRV                                     
         BR    RE                                                               
                                                                                
DXTFLG   DS    0H                  TRADE FLAG                                   
         MVC   EQCTFLG(L'RCONCJTF),RCONCJTF                                     
         BR    RE                                                               
                                                                                
DXPCT1   DS    0H                  PRODUCT CATEGORY #1                          
         MVC   EQCPCT1(L'RCONCJP1),RCONCJP1                                     
         BR    RE                                                               
                                                                                
DXPCT2   DS    0H                  PRODUCT CATEGORY #2                          
         MVC   EQCPCT2(L'RCONCJP2),RCONCJP2                                     
         BR    RE                                                               
                                                                                
DXCTYP   DS    0H                  COOP TYPE                                    
         MVC   EQCCTYP(L'RCONCJCT),RCONCJCT                                     
         BR    RE                                                               
                                                                                
DXCSEP   NTR1                      CONTRACT SEPARATION                          
         CLC   =C'BRK',RCONCJCS    FIELD SET TO 'BREAK'?                        
         BNE   DXCSEP05            NO  -                                        
         MVC   EQCCSEP(3),RCONCJCS YES - INSERT INTO SCREEN                     
         B     DXCSEPX             EXIT ROUTINE                                 
DXCSEP05 EQU   *                                                                
         MVC   WORK(L'RCONCJCS),RCONCJCS                                        
         NI    WORK,X'FF'-X'80'    TURN OF DISPLAY FLAG                         
         ZICM  R3,WORK,3           SETUP TOTAL MINUTES IN R2                    
                                                                                
         TM    RCONCJCS,X'80'      DETERMINE WHICH FORMAT TO USE                
         BO    DXCSEP10                                                         
*                                  DISPLAY HH:MM                                
         SR    R2,R2                                                            
         D     R2,=F'60'                                                        
         EDIT  (R3),(2,EQCCSEP),FILL=0    HOURS                                 
         MVI   EQCCSEP+2,C':'                                                   
         LA    R3,EQCCSEP+3                                                     
         EDIT  (R2),(2,(R3)),FILL=0       MINUTES                               
         B     DXCSEPX                                                          
                                                                                
DXCSEP10 DS    0H                  DISPLAY TOTAL MINUTES                        
         EDIT  (R3),(4,EQCCSEP),ALIGN=LEFT                                      
                                                                                
DXCSEPX  DS    0H                                                               
         B     XIT                                                              
                                                                                
DXSHAN   DS    0H                  SPECIAL HANDLING                             
         MVC   EQCSHAN(L'RCONCJSH),RCONCJSH                                     
         BR    RE                                                               
                                                                                
DXICMT   DS    0H                  INVOICE COMMENTS                             
         MVC   EQCICMT(L'RCONCJIC),RCONCJIC                                     
         BR    RE                                                               
                                                                                
DXIMSC   DS    0H                  INVOICE MESSAGE CODE                         
         MVC   EQCIMSC(L'RCONCJIM),RCONCJIM                                     
         BR    RE                                                               
                                                                                
DXSTAX   DS    0H                  SALES TAX CODE                               
         MVC   EQCSTAX(L'RCONCJST),RCONCJST                                     
         BR    RE                                                               
                                                                                
DXNRL    DS    0H                  NRL                                          
         MVC   EQCNRL(L'RCONCJNR),RCONCJNR                                      
         BR    RE                                                               
                                                                                
DXTORD   DS    0H                  TRAFFIC ORDER #                              
         MVC   EQCTORD(L'RCONCJTO),RCONCJTO                                     
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* VALIDATION ROUTINES                                                           
*******************************************************************             
         USING RCONCJEL,R6                                                      
CXRATE   DS    0H                  RATE                                         
         LA    R2,EQCRATEH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),2                                                          
         BNE   ERROR                                                            
         BR    RE                                                               
                                                                                
CXBCUS   DS    0H                  BILLING CUSTOMER                             
         LA    R2,EQCCUSH                                                       
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),6                                                          
         BNE   ERROR                                                            
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    ERROR                                                            
         BR    RE                                                               
                                                                                
CXBCYC   DS    0H                  BILLING CYCLE                                
         LA    R2,EQCCYCH                                                       
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         CLI   8(R2),C'E'                                                       
         BER   RE                                                               
         CLI   8(R2),C'W'                                                       
         BER   RE                                                               
         B     ERROR                                                            
                                                                                
CXRCAT   DS    0H                  REVENUE CATEGORY                             
         LA    R2,EQCRCATH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         TM    4(R2),X'04'         VALID ALPHBETIC?                             
         BZ    ERROR                                                            
         BR    RE                                                               
                                                                                
CXTFLG   DS    0H                  TRADE FLAG                                   
         LA    R2,EQCTFLGH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         CLI   8(R2),C'X'                                                       
         BNE   ERROR                                                            
         BR    RE                                                               
                                                                                
CXPCT1   DS    0H                  PRODUCT CATEGORY #1                          
         LA    R2,EQCPCT1H                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),2                                                          
         BNE   ERROR                                                            
         BR    RE                                                               
                                                                                
CXPCT2   DS    0H                  PRODUCT CATEGORY #2                          
         LA    R2,EQCPCT2H                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),2                                                          
         BNE   ERROR                                                            
         BR    RE                                                               
                                                                                
CXCTYP   DS    0H                  COOP TYPE                                    
         LA    R2,EQCCTYPH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         CLI   8(R2),C'1'                                                       
         BL    ERROR                                                            
         CLI   8(R2),C'3'                                                       
         BH    ERROR                                                            
         BR    RE                                                               
                                                                                
CXCSEP   NTR1                      CONTRACT SEPARATION                          
         LA    R2,EQCCSEPH                                                      
         CLI   5(R2),0                                                          
         BE    CXCSEPX                                                          
         CLI   5(R2),3             LENGTH = 3?                                  
         BNE   CXCSEP05            NO  - VALIDATE FURTHER                       
         CLC   =C'BRK',8(R2)       YES - 'BREAK' VALUE?                         
         BNE   CXCSEP05            NO  - VALIDATE FURTHER                       
         MVC   CONTSEP,8(R2)       YES - SAVE THIS VALUE                        
         B     CXCSEPX             EXIT ROUTINE                                 
CXCSEP05 EQU   *                                                                
         GOTO1 SCANNER,DMCB,EQCCSEPH,(2,WORK2),C',=::'                          
         CLI   DMCB+4,2                                                         
         BE    CXCSEP10                                                         
         CLI   DMCB+4,1                                                         
         BNE   ERROR                                                            
                                                                                
         CLI   WORK2,4             INPUT IS TOTAL MINUTES                       
         BH    ERROR                                                            
         TM    WORK2+2,X'80'                                                    
         BZ    ERROR               MUST BE NUMERIC                              
         MVC   CONTSEP,WORK2+5                                                  
         OI    CONTSEP,X'80'       FLAG AS TOTAL MINUTES                        
         B     CXCSEPX                                                          
                                                                                
CXCSEP10 DS    0H                  INPUT IS HH:MM                               
         CLI   WORK2,2             HOURS CANNOT BE > 99                         
         BH    ERROR                                                            
         TM    WORK2+2,X'80'                                                    
         BZ    ERROR               MUST BE NUMERIC                              
                                                                                
         CLI   WORK2+32,2          MINUTES CANNOT BE > 59                       
         BH    ERROR                                                            
         TM    WORK2+34,X'80'                                                   
         BZ    ERROR               MUST BE NUMERIC                              
         CLC   WORK2+36,=F'59'                                                  
         BH    ERROR                                                            
                                                                                
         ICM   R0,15,WORK2+4       HOURS                                        
         MH    R0,=H'60'           R0 HAS TOTAL HOUR TO MINUTES                 
                                                                                
         ICM   R1,15,WORK2+36      MINUTES                                      
         AR    R0,R1               TOTAL MINUTES                                
         STCM  R0,7,CONTSEP                                                     
                                                                                
CXCSEPX  DS    0H                                                               
         B     XIT                                                              
                                                                                
CXSHAN   DS    0H                  SPECIAL HANDLING                             
         LA    R2,EQCSHANH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         TM    4(R2),X'04'         VALID ALPHBETIC?                             
         BZ    ERROR                                                            
         BR    RE                                                               
                                                                                
CXICMT   DS    0H                  INVOICE COMMENTS                             
         LA    R2,EQCICMTH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR                                                            
         BR    RE                                                               
                                                                                
CXIMSC   DS    0H                  INVOICE MESSAGE CODE                         
         LA    R2,EQCIMSCH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),2                                                          
         BNE   ERROR                                                            
         BR    RE                                                               
                                                                                
CXSTAX   DS    0H                  SALES TAX CODE                               
         LA    R2,EQCSTAXH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         TM    4(R2),X'04'         VALID ALPHBETIC?                             
         BZ    ERROR                                                            
         BR    RE                                                               
                                                                                
CXNRL    DS    0H                  NRL                                          
         LA    R2,EQCNRLH                                                       
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),1                                                          
         BNE   ERROR                                                            
         CLI   8(R2),C'N'                                                       
         BER   RE                                                               
         CLI   8(R2),C'R'                                                       
         BER   RE                                                               
         CLI   8(R2),C'L'                                                       
         BER   RE                                                               
         B     ERROR                                                            
                                                                                
CXTORD   DS    0H                  TRAFFIC ORDER #                              
         LA    R2,EQCTORDH                                                      
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   5(R2),6                                                          
         BH    ERROR                                                            
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* RETRIEVE STATION OF CONTRACT.  STORE OFF TRAFFIC SYSTEM CODE                  
*******************************************************************             
STATION  NTR1                                                                   
         LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP-RSTAREC(R6),2            RECORD TYPE                    
         MVC   RSTAKREP-RSTAREC(2,R6),RCONKREP   REP CODE                       
         MVC   RSTAKSTA-RSTAREC(5,R6),RCONKSTA   STATION CODE                   
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                STATION MUST BE ON FILE                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING RSTAREC,R6                                                       
         LA    R2,TRAFTABL         SET A(TRAFFIC TABLE)                         
STAT0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    STAT0100            YES - FORCE DUMP                             
         CLC   RSTATRAF,0(R2)      STATION TRAFFIC = TABLE CODE?                
         BE    STAT0040            YES -                                        
         LA    R2,2(R2)            NO  - BUMP TO NEXT ENTRY                     
         B     STAT0020            GO BACK FOR NEXT                             
STAT0040 EQU   *                                                                
         CLI   1(R2),0             EOP SYSTEM CODE EMPTY?                       
         BE    STAT0100            YES - FORCE DUMP                             
         MVC   TRAFCODE,1(R2)      NO  - SAVE EOP RECORD CODE                   
         XIT1                                                                   
STAT0100 EQU   *                                                                
         DC    H'0'                DUMP:  UNRECOGNIZED CODE, OR                 
*                                     DON'T BELONG HERE.                        
*                                                                               
*   TRAFTABL:  1ST BYTE = TRAFFIC CODE FROM STATION RECORD                      
*              2ND BYTE = EOP RECORD TRAFFIC SYSTEM CODE                        
*                         0  =  DON'T PERMIT                                    
TRAFTABL DC    C'B',X'0'           BIAS                                         
         DC    C'W',X'0'           BIAS                                         
         DC    C'J',X'2'           JDS/2000                                     
         DC    C'K',X'3'           ENTERPRISE                                   
         DC    C'H',X'3'           ENTERPRISE II                                
         DC    C'C',X'4'           COLUMBINE                                    
         DC    C'V',X'0'           VCI                                          
         DC    X'0000'                                                          
         DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* RETRIEVE CORRESPONDING EOP RECORD FOR EOP DISPLAY                             
* WILL ALSO PROTECT FIELD IF ACTIVE FLAG IS SET                                 
* P1=(B1)RECORD TYPE, (B2-3)SCREEN HEADER ADDRESS FOR DISPLAY                   
* P2=(B1)LENGTH OF EOP FIELD, (B2-3) ADDRESS OF EOP FIELD                       
* P3=(B1)OFFSET TO FIRST NON-NULL FIELD OF EOP KEY                              
*    (B2-3)LENGTH OF SCREEN DATA FIELD                                          
*******************************************************************             
GETEOP   NTR1                                                                   
         MVC   WORK(16),0(R1)                                                   
         XC    HALF,HALF           CLEAR PASS-BACK FIELD                        
*                                                                               
*   PASS-BACK FIELD IS USED FOR TRADE AGENCY PROCESSING.  THIS WILL             
*        CONTAIN THE ALTERNATE TRADE OFFICE CODE IF A TRADE                     
*        ORDER'S AGENCY CODE MUST BE ASSIGNED.                                  
*        DO NOT - REPEAT, NOT - STEP ON THE FIRST BYTE OF HALF                  
*        IN THIS ROUTINE.                                                       
*                                                                               
         CLI   WORK+15,0           P4 (2-4) ZERO? (NOT AGENCY)                  
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
         MVC   2(1,R6),TRAFCODE    INSERT TRAFFIC CODE                          
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
         ZICM  R1,WORK+9,3                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,RF),REOPEQUV                                                 
                                                                                
         TM    REOPFLAG,X'80'      IF CODE ACTIVE, PROTECT FIELD                
         BZ    GETEOPX                                                          
         OI    6(RF),X'20'                                                      
                                                                                
GETEOPX  DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
* RETRIEVE CORRESPONDING EOP RECORD FOR EOP CHANGE                              
* P1=(B1)RECORD TYPE, (B2-4)SCREEN HEADER ADDRESS WITH DATA                     
* P2=(B1)LENGTH OF EOP FIELD, (B2-4) ADDRESS OF EOP FIELD                       
* P3=(B1)OFFSET TO FIRST NON-NULL FIELD OF EOP KEY                              
* P3=(B2-4)DIFFERING LENGTH OF CODE FIELDS                                      
* P4=(B2-4)NON-ZERO INDICATES AGENCY CODE PROCESSING                            
*******************************************************************             
CHGEOP   NTR1                                                                   
         MVC   WORK(16),0(R1)                                                   
                                                                                
         XC    HALF,HALF           CLEAR PASS-BACK FIELD                        
*                                                                               
*   PASS-BACK FIELD IS USED FOR TRADE AGENCY PROCESSING.  THIS WILL             
*        CONTAIN THE ALTERNATE TRADE OFFICE CODE IF A TRADE                     
*        ORDER'S AGENCY CODE MUST BE ASSIGNED.                                  
*        DO NOT - REPEAT, NOT - STEP ON THE FIRST BYTE OF HALF                  
*        IN THIS ROUTINE.                                                       
*                                                                               
         CLI   WORK+15,0           P4 (2-4) ZERO? (NOT AGENCY)                  
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
         MVC   2(1,R6),TRAFCODE    INSERT TRAFFIC CODE                          
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
                                                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
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
         CLI   WORK+15,0           P3 (2-4) ZERO? (NOT AGENCY)                  
         BE    CHGE0040            YES - NOT AGENCY                             
         MVC   REOPTROF,RCONKAOF   INSERT AGENCY OFFICE                         
CHGE0040 EQU   *                                                                
                                                                                
         XC    REOPEQUV,REOPEQUV                                                
         CLI   TRAFCODE,4          COLUMBINE?                                   
         BE    CHGEOP05            YES - TAKES NUMERIC CODES                    
         CLI   REOPKEY,X'1B'       NO  - TAKES A/N EOP FIELDS                   
         BE    CHGEOP15                                                         
         CLI   REOPKEY,X'1D'                                                    
         BE    CHGEOP15                                                         
CHGEOP05 EQU   *                                                                
         ZICM  R2,WORK+1,3         TWA ADDRESS                                  
         ZICM  RF,WORK+9,3         FIELD DSECT LENGTH                           
         ZIC   R1,5(R2)            ACTUAL FIELD LENGTH ON SCREEN                
                                                                                
         LA    RE,REOPEQUV                                                      
         AR    RE,RF                                                            
         SR    RE,R1                                                            
         BCTR  R1,0                                                             
         BCTR  RF,0                                                             
         EX    RF,CHGEOP10                                                      
         EX    R1,CHGEOP13                                                      
         B     CHGEOP40                                                         
CHGEOP10 MVC   REOPEQUV(0),=6C'0'                                               
CHGEOP13 MVC   0(0,RE),8(R2)                                                    
                                                                                
CHGEOP15 DS    0H                                                               
         ZICM  RF,WORK+1,3                                                      
         CLI   REOPKEY,X'1B'       ADVERTISER CODE?                             
         BNE   CHGEOP19            NO                                           
         CLI   5(RF),4             MAX LENGTH EXCEEDED FOR JDS/2000?            
         BNH   CHGEOP19            NO                                           
         LA    R3,28               ERROR, INPUT TOO LONGE                       
         B     ERROR                                                            
CHGEOP19 EQU   *                                                                
         ZICM  R1,WORK+9,3                                                      
         BCTR  R1,0                                                             
         EX    R1,CHGEOP20                                                      
         EX    R1,CHGEOP30                                                      
         B     CHGEOP40                                                         
CHGEOP20 MVC   REOPEQUV(0),8(RF)                                                
CHGEOP30 OC    REOPEQUV(0),MYSPACES                                             
         DROP  R6                                                               
                                                                                
CHGEOP40 DS    0H                                                               
         GOTO1 VADDREC,DMCB,IOAREA ADD THE RECORD                               
         B     CHGEOPX                                                          
                                                                                
CHGEOP50 DS    0H                  CHANGE EXISTING RECORD                       
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         USING REOPREC,R6                                                       
                                                                                
         TM    REOPFLAG,X'80'      CODE ACTIVE IN ORDER                         
         BZ    CHGEOP55                                                         
         LA    R3,406              ERROR, CANNOT CHANGE AN ACTIVE CODE          
         B     ERROR                                                            
                                                                                
CHGEOP55 DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,REOPDATE)                                   
         XC    REOPEQUV,REOPEQUV                                                
         CLI   TRAFCODE,4          COLUMBINE?                                   
         BE    CHGEOP57            YES - TAKES NUMERIC CODES                    
         CLI   REOPKEY,X'1B'       THESE TAKE ALPHNUMERIC EOP FIELDS            
         BE    CHGEOP80                                                         
         CLI   REOPKEY,X'1D'                                                    
         BE    CHGEOP80                                                         
CHGEOP57 EQU   *                                                                
         ZICM  R2,WORK+1,3                                                      
         ZICM  RF,WORK+9,3                                                      
         ZIC   R1,5(R2)                                                         
         LA    RE,REOPEQUV                                                      
         AR    RE,RF                                                            
         SR    RE,R1                                                            
         BCTR  RF,0                                                             
         BCTR  R1,0                                                             
         EX    RF,CHGEOP60                                                      
         EX    R1,CHGEOP70                                                      
         B     CHGEOP95                                                         
CHGEOP60 MVC   REOPEQUV(0),=6C'0'                                               
CHGEOP70 MVC   0(0,RE),8(R2)                                                    
                                                                                
CHGEOP80 DS    0H                                                               
         ZICM  RF,WORK+1,3                                                      
         CLI   REOPKEY,X'1B'       ADVERTISER CODE FOR JDS/2000?                
         BNE   CHGEOP82            NO                                           
         CLI   5(RF),4             YES - MAX LEN EXCEEDED FOR JDS/2000?         
         BNH   CHGEOP82            NO                                           
         LA    R3,28               ERROR, INPUT TOO LONGE                       
         B     ERROR                                                            
CHGEOP82 EQU   *                                                                
         ZICM  R1,WORK+9,3                                                      
         BCTR  R1,0                                                             
         EX    R1,CHGEOP85                                                      
         EX    R1,CHGEOP90                                                      
         B     CHGEOP95                                                         
CHGEOP85 MVC   REOPEQUV(0),8(RF)                                                
CHGEOP90 OC    REOPEQUV(0),MYSPACES                                             
                                                                                
CHGEOP95 DS    0H                                                               
         NI    REOPCNTL,X'FF'-X'80' RESTORE INCASE RECORD IS DELETED            
                                                                                
         GOTO1 VPUTREC,DMCB,IOAREA                                              
                                                                                
         TM    KEY+27,X'80'        RESTORE DELETED RECORD                       
         BZ    CHGEOPX                                                          
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE                                                           
                                                                                
CHGEOPX  DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
XIT      XIT1                                                                   
                                                                                
*******************************************************************             
* LOCAL VARIABLES                                                               
*******************************************************************             
SVPROF   DS    CL16                PROFILE INFO                                 
CONTSEP  DS    CL(L'RCONCJCS)      CONTRACT SEPARATION VALUE                    
TRAFCODE DS    CL1                 TRAFFIC CODE                                 
         EJECT                                                                  
*******************************************************************             
* TABLE TO BRANCH TO DISPLAY/EDIT ROUTINES AS DICTATED BY GETPROF               
*   RETURNED PROFILES                                                           
*******************************************************************             
         DS    0F                                                               
JDSDXCX  DS    0CL9                                                             
         DC    AL4(DXRATE),AL4(CXRATE),AL1(2)                                   
         DC    AL4(DXBCUS),AL4(CXBCUS),AL1(6)                                   
         DC    AL4(DXBCYC),AL4(CXBCYC),AL1(1)                                   
         DC    AL4(DXRCAT),AL4(CXRCAT),AL1(1)                                   
         DC    AL4(DXTFLG),AL4(CXTFLG),AL1(1)                                   
         DC    AL4(DXPCT1),AL4(CXPCT1),AL1(2)                                   
         DC    AL4(DXPCT2),AL4(CXPCT2),AL1(2)                                   
         DC    AL4(DXCTYP),AL4(CXCTYP),AL1(1)                                   
         DC    AL4(DXCSEP),AL4(CXCSEP),AL1(4)                                   
         DC    AL4(DXSHAN),AL4(CXSHAN),AL1(1)                                   
         DC    AL4(DXICMT),AL4(CXICMT),AL1(1)                                   
         DC    AL4(DXIMSC),AL4(CXIMSC),AL1(2)                                   
         DC    AL4(DXSTAX),AL4(CXSTAX),AL1(1)                                   
         DC    AL4(DXNRL),AL4(CXNRL),AL1(1)                                     
         DC    AL4(DXTORD),AL4(CXTORD),AL1(6)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONLAST                                                          
       ++INCLUDE RECNTE9D                                                       
         CSECT                                                                  
       ++INCLUDE RETRADE                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016RECNT47   05/01/02'                                      
         END                                                                    
