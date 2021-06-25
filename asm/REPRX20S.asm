*          DATA SET REPRX20S   AT LEVEL 019 AS OF 11/01/99                      
*PHASE T81A20A                                                                  
*                                                                               
T81A20   TITLE 'REPRP20 - INVENTORY DOWNLOAD ROUTINES'                          
*********************************************************************           
*                                                                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
*  HISTORY AS OF 2/10/99                                              *         
*                                                                     *         
* 02/10/1999  JRD    ADD HARDCODED HISPANIC STATION SWITCH FOR TEL-H  *         
* 03/02/1999  JRD    SUPPORT DOWNLOAD OF STATIONS FOR RATECARDS       *         
* 05/25/1999  JRD    CHECK FOR TEXT OF ALL SPACES                     *         
* 06/24/1999  JRD    CHECK FOR TEXT OF ALL SPACES NOT JUST IN FILTERS *         
*                    CHECK FOR USER ENTRING NO TEXT IN THE TEXT       *         
* 10/19/1999  JRD    CHANGE -CC TIMES TO 2A                           *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
T81A20   CSECT                                                                  
         NMOD1 OVERWRKQ,*T81A20*,RR=RE,CLEAR=YES                                
         LR    R9,RC                                                            
         USING OVERWRKD,R9                                                      
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         L     RA,AMAPTAB                                                       
         USING MAPTABD,RA                                                       
*                                                                               
         MVC   OVPARMS,0(R1)                                                    
         ST    RE,OVRELO                                                        
*                                                                               
         LR    R8,RB                                                            
         AH    R8,=Y(COMMON-T81A20)                                             
         USING COMMON,R8                                                        
*                                                                               
         SRL   RF,32-8                                                          
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SLL   RF,2                                                             
         L     RF,ROUTS(RF)                                                     
         A     RF,OVRELO                                                        
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
ROUTS    DS    0F                                                               
         DC    A(NEWINV)                                                        
         DC    A(NEWDATA)                                                       
         DC    A(NEWTEXT)                                                       
         DC    A(RCDLIST)                                                       
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
COMMON   DS    0D                                                               
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
ETOOBIG  MVC   ERROR,=Y(804)                                                    
         B     EXITL                                                            
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     DS    0H                                                               
         XIT1  ,                   EXIT WITH CC SET                             
DIE      DC    H'0'                                                             
***********************************************************************         
* CHECK FOR A HISPANIC STATION CALL LETTER SWITCH                               
*                                                                               
*  R1 A(STATION CALL LETTERS)                                                   
*                                                                               
*  TEL-H --> TELE-T ON EJOR(B3) AND TELEMUNDO(B1)                               
*                                                                               
* IMPORTANT!!!! MUST MATCH SWITCH IN REPRP00!!!!!!!!!!!!!!!                     
*                                                                               
***********************************************************************         
SWHISP   DS    0H                                                               
         CLC   REPALPHA,=C'B3'     TELE-H                                       
         BE    *+14                                                             
         CLC   REPALPHA,=C'B1'                                                  
         BNE   SWH010                                                           
*                                                                               
         CLC   =C'TEL H',0(R1)                                                  
         BNE   SWH010                                                           
         MVC   0(5,R1),=C'TELE '                                                
         B     SWHISPX                                                          
*                                                                               
SWH010   DS    0H                                                               
*                                                                               
SWHISPX  DS    0H                                                               
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
LONGPARM EQU   X'FF'               FETCH INDICATOR FOR ADDR. PRAMETER           
INVREJ   EQU   RFTRBADQ            FETCH INDICATOR INV HDR REJECT               
FTCHWDTH EQU   132                                                              
*                                                                               
STALENQ  EQU   5                                                                
DPTLENQ  EQU   1                                                                
DEMLENQ  EQU   4                                                                
BKLENQ   EQU   6                                                                
UPGLENQ  EQU   11+14+1                                                          
RCDLENQ  EQU   8+1+1                                                            
FLTLENQ  EQU   6                                                                
         EJECT                                                                  
*********************************************************************           
*********************************************************************           
RCDLIST  NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
K        USING RARTREC,KEY                                                      
         MVI   K.RARTKTYP,X'3E'                                                 
         MVC   K.RARTKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
RCDL000  DS    0H                                                               
         CLC   KEY(RARTKCOD-RARTREC),KEYSAVE                                    
         BNE   RCDLX                                                            
*                                                                               
         CLI   KEY+(RARTKCOD-RARTREC)+(L'RARTKCOD-1),0                          
         BE    RCDL040             OLD RECORD, SKIP IT                          
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         USING RARTREC,R6                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,RCDDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCDNAMEL,RARTKCOD,0                         
*                                                                               
         LA    R6,RARTPEL                                                       
RCDL010  DS    0H                                                               
         CLI   0(R6),0             END OF RECORD?                               
         BE    RCDL040             YES                                          
         CLI   0(R6),X'02'         LENGTH/QUARTER ELEMENT?                      
         BNE   RCDL020             NO                                           
*                                                                               
         USING RALQELEM,R6                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,RCDYREL,RALQYEAR,0                          
         LA    R0,RCDLENEL                                                      
         TM    RALQSTAT,X'80'      LENGTH IN MINUTES?                           
         BZ    *+8                 NO                                           
         LA    R0,RCDMLNEL                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,(R0),RALQLEN,0                              
         GOTO1 AADDDATA,DMCB,AFABLK,RCDQTREL,RALQQTR,0                          
*                                                                               
         TM    RALQQTR,X'80'                                                    
         BZ    RCDL012                                                          
         OC    RALQLST1,RALQLST1                                                
         BZ    RCDL012                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCDQ1LEL,RALQLST1,0                         
*                                                                               
RCDL012  DS    0H                                                               
         TM    RALQQTR,X'40'                                                    
         BZ    RCDL014                                                          
         OC    RALQLST2,RALQLST2                                                
         BZ    RCDL014                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCDQ2LEL,RALQLST2,0                         
*                                                                               
RCDL014  DS    0H                                                               
         TM    RALQQTR,X'20'                                                    
         BZ    RCDL016                                                          
         OC    RALQLST3,RALQLST3                                                
         BZ    RCDL016                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCDQ3LEL,RALQLST3,0                         
*                                                                               
RCDL016  DS    0H                                                               
         TM    RALQQTR,X'10'                                                    
         BZ    RCDL030                                                          
         OC    RALQLST4,RALQLST4                                                
         BZ    RCDL030                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCDQ4LEL,RALQLST4,0                         
*                                                                               
RCDL020  DS    0H                                                               
         CLI   0(R6),X'03'         STATION ELEMENT?                             
         BNE   RCDL030             NO                                           
*                                                                               
         TM    MISCFLG1,MF1RST     STATION INFO REQUESTED?                      
         BZ    RCDL030             NO                                           
*                                                                               
         USING RASTELEM,R6                                                      
         XC    WORK,WORK                                                        
         MVC   WORK+20(5),RASTSTA                                               
         CLI   WORK+24,C'T'                                                     
         BNE   *+8                                                              
         MVI   WORK+24,C' '                                                     
         LA    R1,WORK+20          SWITCH HISPANIC CALL LETTERS BACK            
         BAS   RE,SWHISP                                                        
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCDSTAEL,WORK+20,0                          
*                                                                               
RCDL030  DS    0H                                                               
         ZIC   R0,1(R6)            NEXT ELEMENT                                 
         AR    R6,R0                                                            
         B     RCDL010                                                          
*                                                                               
RCDL040  DS    0H                                                               
         GOTO1 VSEQ                                                             
         B     RCDL000                                                          
*                                                                               
RCDLX    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
*********************************************************************           
NEWINV   NTR1  BASE=*,LABEL=*                                                   
         MVI   MODE,C'I'           NEW INVENTORY MODE                           
         L     R2,OVPARMS+4                                                     
*                                                                               
         MVC   NUMSTAS,0(R2)       NUMBER OF STATIONS                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST STATION                         
         ST    RF,FRSTSTA                                                       
         ZIC   RF,NUMSTAS                                                       
         MH    RF,=Y(STALENQ)      BUMP TO DAYPARTS                             
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMDPTS,0(R2)       NUMBER OF DAYPARTS                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST DAYPART                         
         ST    RF,FRSTDPT                                                       
         ZIC   RF,NUMDPTS                                                       
         MH    RF,=Y(DPTLENQ)      BUMP TO FLIGHTS                              
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMFLTS,0(R2)       NUMBER OF FLIGHTS                            
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST FLIGHT                          
         ST    RF,FRSTFLT                                                       
         MVC   FLTSTART,0(R2)      SAVE START OF TOTAL FLIGHT                   
         ZIC   RF,NUMFLTS                                                       
         BCTR  RF,0                BUMP TO LAST FLIGHT                          
         MH    RF,=Y(6)                                                         
         AR    R2,RF                                                            
         MVC   FLTEND,3(R2)        SAVE END OF TOTAL FLIGHT                     
         AH    R2,=Y(FLTLENQ)      BUMP TO BOOKS                                
*                                                                               
         MVC   NUMBKS,0(R2)        NUMBER OF BOOKS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST BOOK                            
         ST    RF,FRSTBK                                                        
         ZIC   RF,NUMBKS                                                        
         MH    RF,=Y(BKLENQ)       BUMP TO UPGRADES                             
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMUPGS,0(R2)       NUMBER OF UPGRADES                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST UPGRADE                         
         ST    RF,FRSTUPG                                                       
         ZIC   RF,NUMUPGS                                                       
         MH    RF,=Y(UPGLENQ)      BUMP TO DEMOS                                
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMDEMS,0(R2)       NUMBER OF DEMOS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST DEMO                            
         ST    RF,FRSTDEM                                                       
         ZIC   RF,NUMDEMS                                                       
         MH    RF,=Y(DEMLENQ)      BUMP TO RATE CARDS                           
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMRCDS,0(R2)       NUMBER OF RATE CARDS                         
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST RATE CARD                       
         ST    RF,FRSTRCD                                                       
         ZIC   RF,NUMRCDS                                                       
         MH    RF,=Y(RCDLENQ)                                                   
         AR    R2,RF                                                            
         EJECT                                                                  
*                                                                               
         MVC   CURSTA,FRSTSTA      SET CURRENT STATION                          
         MVC   REMSTAS,NUMSTAS     SET NUMBER OF REMAINING STATIONS             
*                                                                               
         CLI   NUMSTAS,0                                                        
         BE    EXITOK                                                           
*                                                                               
         CLI   NUMDPTS,0                                                        
         BE    EXITOK                                                           
*                                  SET NO DATA                                  
********************************************************                        
* LOOP THROUGH ALL THE STATIONS                                                 
********************************************************                        
LPSTA000 DS    0H                                                               
         LA    R0,FETCHBLK         CLEAR THE FETCH PARAMTER BLOCK               
         LH    R1,=Y(RFTBLKL)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOMFACS    A(COMFACS)                                   
         MVC   RFTAIO1,AIO3        A(2K IO AREA)                                
         MVC   RFTAIO2,AIO4        A(2K IO AREA)                                
         LA    RE,FETCHWRK                                                      
         STCM  RE,15,RFTAWRK       A(6K WORK AREA)                              
         MVC   RFTCREP,REPALPHA    REP CODE                                     
         MVC   RFTCSRC,RTSRVC      RATING SERVICE                               
         MVI   RFTAMODE,RFTAINVQ   FETCH MODE                                   
         MVI   RFTCDCTL,RFTCDC1Q   FETCH METHOD                                 
         LA    RE,FTCHHOOK         HOOK ROUTINE                                 
         STCM  RE,15,RFTHOOKA                                                   
         OI    RFTCNTL,RFTCHDRQ    INCLUDE HEADERS                              
         OI    RFTCNTL,RFTCDEMQ    INCLUDE DEMOS                                
         OI    RFTCNTL,RFTCSLVQ    INCLUDE SHARES AND LEVELS                    
         OI    RFTCNTL,RFTCFTNQ    INCLUDE FOOTNOTES                            
         OI    RFTCNTL,RFTCRTEQ    INCLUDE RATES                                
         OI    RFTCNTL,RFTCRNWQ    USE NEW STYLE RATES                          
*                                                                               
         GOTO1 VDATCON,DMCB,(8,FLTSTART),(2,RFTCEFST)                           
         GOTO1 VDATCON,DMCB,(8,FLTEND),(2,RFTCEFEN)                             
*                                                                               
         L     RE,OVPARMS+4        POINT TO CURRENT STATIONS                    
         A     RE,CURSTA                                                        
         MVC   RFTCSTAT,0(RE)      STATION CALL LETTERS                         
         CLI   RFTCSTAT+4,C' '     NEED 'T' SET?                                
         BNE   *+8                 NO                                           
         MVI   RFTCSTAT+4,C'T'                                                  
*                                                                               
         L     RF,AIO1                                                          
         ST    RF,ACURPARM         STORE ADDRESS OF CURRENT PARMS               
         EJECT                                                                  
*--------------------*                                                          
* BUILD DAYPART LIST *                                                          
*--------------------*                                                          
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         MVI   RFTCDTMS,LONGPARM   SET PARM AS NULL TERM. LIST                  
         STCM  RF,15,RFTCDTMS+1                                                 
         ZIC   R1,NUMDPTS          SET NUMBER OF REMAINING DAYPARTS             
         L     RE,OVPARMS+4        POINT TO CURRENT DAYPART                     
         A     RE,FRSTDPT                                                       
DPTS010  DS    0H                  LOOP AND SET DAYPARTS                        
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCDTLQ*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCDTMS,RF                                                      
         XC    0(RFTCDTLQ,RF),0(RF)                                             
         MVC   RFTCDTDP,0(RE)                                                   
         LA    RF,RFTCDTLQ(RF)                                                  
         LA    RE,DPTLENQ(RE)                                                   
         DROP  RF                                                               
*                                                                               
         BCT   R1,DPTS010                                                       
*                                                                               
         XC    0(RFTCDTLQ,RF),0(RF)                                             
         LA    RF,RFTCDTLQ(RF)                                                  
         ST    RF,ACURPARM                                                      
*                                                                               
         EJECT                                                                  
*-----------------*                                                             
* BUILD BOOK LIST *                                                             
*-----------------*                                                             
         CLI   NUMDEMS,0                                                        
         BE    DEMS100                                                          
*                                                                               
         CLI   NUMBKS,0                                                         
         BE    BKS0100                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         MVI   RFTCBKS,LONGPARM    SET PARM AS NULL TERM. LIST                  
         STCM  RF,15,RFTCBKS+1                                                  
         ZIC   R1,NUMBKS           SET NUMBER OF REMAINING BOOKS                
         L     RE,OVPARMS+4        POINT TO CURRENT BOOK                        
         A     RE,FRSTBK                                                        
BKS0010  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCBKLQ*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCBKS,RF                                                       
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         MVC   0(RFTCBKLQ,RF),0(RE)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         LA    RE,BKLENQ(RE)                                                    
         DROP  RF                                                               
*                                                                               
         BCT   R1,BKS0010                                                       
*                                                                               
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         ST    RF,ACURPARM                                                      
*                                                                               
BKS0100  DS    0H                                                               
         EJECT                                                                  
*--------------------*                                                          
* BUILD UPGRADE LIST *                                                          
*--------------------*                                                          
         CLI   NUMUPGS,0                                                        
         BE    UPGS100                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         STCM  RF,15,RFTCUPGA                                                   
         ZIC   R1,NUMUPGS          SET NUMBER OF REMAINING BOOKS                
         L     RE,OVPARMS+4        POINT TO CURRENT BOOK                        
         A     RE,FRSTUPG                                                       
UPGS010  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(UPGLENQ*2))                                         
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         MVC   0(UPGLENQ-1,RF),0(RE)                                            
         LA    RF,UPGLENQ-1(RF)                                                 
         LA    RE,UPGLENQ(RE)                                                   
         BCT   R1,UPGS010                                                       
*                                                                               
         XC    0(UPGLENQ,RF),0(RF)                                              
         LA    RF,UPGLENQ(RF)                                                   
         ST    RF,ACURPARM                                                      
*                                                                               
UPGS100  DS    0H                                                               
         EJECT                                                                  
*-----------------*                                                             
* BUILD DEMO LIST *                                                             
*-----------------*                                                             
         LA    RF,RFTCDEMS                                                      
         ZIC   R1,NUMDEMS          SET NUMBER OF REMAINING DEMOS                
         CLI   NUMDEMS,((RFTCBKS-RFTCDEMS)/3)-1                                 
         BNH   *+8                 NOT TOO MANY DEMOS                           
         LA    R1,((RFTCBKS-RFTCDEMS)/3)-1                                      
         L     RE,OVPARMS+4        POINT TO CURRENT DEMO                        
         A     RE,FRSTDEM                                                       
DEMS010  DS    0H                  LOOP AND SET DEMOS                           
         MVC   0(3,RF),0(RE)                                                    
         LA    RF,3(RF)                                                         
         LA    RE,DEMLENQ(RE)                                                   
         BCT   R1,DEMS010                                                       
*                                                                               
DEMS100  DS    0H                                                               
         EJECT                                                                  
*----------------------*                                                        
* BUILD RATE CARD LIST *                                                        
*----------------------*                                                        
RCDS000  DS    0H                  LOOP AND SET RATE CARDS                      
         CLI   NUMRCDS,0                                                        
         BE    RCDS100                                                          
*                                                                               
         MVC   RFTCRDDT(3),FLTSTART                                             
         MVC   RFTCRDDT+3(3),FLTEND                                             
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         STCM  RF,15,RFTCRDRC                                                   
         ZIC   R1,NUMRCDS          SET NUMBER OF REMAINING RATE CARDS           
         L     RE,OVPARMS+4        POINT TO CURRENT RATE CARD                   
         A     RE,FRSTRCD                                                       
RCDS010  DS    0H                  LOOP AND SET RATES                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCRTSL*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCRTES,RF                                                      
         XC    0(RCDLENQ,RF),0(RF)                                              
         MVC   0(RCDLENQ,RF),0(RE)                                              
         LA    RF,RCDLENQ(RF)                                                   
         LA    RE,RCDLENQ(RE)                                                   
         DROP  RF                                                               
*                                                                               
         BCT   R1,RCDS010                                                       
*                                                                               
         XC    0(RCDLENQ,RF),0(RF)                                              
         LA    RF,RCDLENQ(RF)                                                   
         ST    RF,ACURPARM                                                      
*                                                                               
RCDS100  DS    0H                                                               
         EJECT                                                                  
*------------*                                                                  
* FETCH CALL *                                                                  
*------------*                                                                  
         CLI   MODE,C'I'                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VFETCH,DMCB,FETCHBLK                                             
         DROP  R4                                                               
*                                                                               
LPSTA100 DS    0H                  PROCESS NEXT STATION                         
         ZIC   R0,REMSTAS          REMAINING STATIONS                           
         BCTR  R0,0                                                             
         STC   R0,REMSTAS                                                       
         CLI   REMSTAS,0           ANYMORE STATIONS?                            
         BNH   LPSTAX              NO                                           
*                                                                               
         L     RE,CURSTA           POINT TO CURRENT STATION                     
         LA    RE,5(RE)            BUMP TO NEXT STATION                         
         ST    RE,CURSTA                                                        
         NI    MISCFLG1,FF-MF1TMPB1   SET NEW STATION                           
         B     LPSTA000               FETCH IT                                  
*                                                                               
LPSTAX   DS    0H                  END OF FETCH LOOP                            
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*********************************************************************           
NEWDATA  NTR1  BASE=*,LABEL=*                                                   
         MVI   MODE,C'D'           NEW DATA MODE                                
         L     R2,OVPARMS+4                                                     
*                                                                               
         MVC   NUMFLTS,0(R2)       NUMBER OF FLIGHTS                            
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST FLIGHT                          
         ST    RF,FRSTFLT                                                       
         MVC   FLTSTART,0(R2)      SAVE START OF TOTAL FLIGHT                   
         ZIC   RF,NUMFLTS                                                       
         BCTR  RF,0                BUMP TO LAST FLIGHT                          
         MH    RF,=Y(6)                                                         
         AR    R2,RF                                                            
         MVC   FLTEND,3(R2)        SAVE END OF TOTAL FLIGHT                     
         AH    R2,=Y(FLTLENQ)      BUMP TO BOOKS                                
*                                                                               
         MVC   NUMBKS,0(R2)        NUMBER OF BOOKS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST BOOK                            
         ST    RF,FRSTBK                                                        
         ZIC   RF,NUMBKS                                                        
         MH    RF,=Y(BKLENQ)       BUMP TO UPGRADES                             
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMUPGS,0(R2)       NUMBER OF UPGRADES                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST UPGRADE                         
         ST    RF,FRSTUPG                                                       
         ZIC   RF,NUMUPGS                                                       
         MH    RF,=Y(UPGLENQ)      BUMP TO DEMOS                                
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMDEMS,0(R2)       NUMBER OF DEMOS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST DEMO                            
         ST    RF,FRSTDEM                                                       
         ZIC   RF,NUMDEMS                                                       
         MH    RF,=Y(DEMLENQ)      BUMP TO RATE CARDS                           
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMRCDS,0(R2)       NUMBER OF RATE CARDS                         
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST RATE CARD                       
         ST    RF,FRSTRCD                                                       
         ZIC   RF,NUMRCDS                                                       
         MH    RF,=Y(RCDLENQ)                                                   
         AR    R2,RF                                                            
         EJECT                                                                  
*                                                                               
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST STATION                         
         ST    RF,FRSTSTA                                                       
         LA    R2,5(R2)            POINT TO STATIONS INVENTORY                  
*                                                                               
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO INVENOTRY                           
         ST    RF,CURINV                                                        
*                                                                               
         MVC   CURSTA,FRSTSTA      SET CURRENT STATION                          
         ZAP   INVSEQ,=P'1'                                                     
*DHAB    NI    MISCFLG1,FF-MF1TMPB1   SET NEW STATION                           
         OI    MISCFLG1,MF1TMPB1      SET OLD STATION                           
*                                                                               
********************************************************                        
* LOOP THROUGH ALL THE STATIONS / INVENTORY                                     
********************************************************                        
XPSTA000 DS    0H                                                               
         LA    R0,FETCHBLK         CLEAR THE FETCH PARAMTER BLOCK               
         LH    R1,=Y(RFTBLKL)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOMFACS    A(COMFACS)                                   
         MVC   RFTAIO1,AIO3        A(2K IO AREA)                                
         MVC   RFTAIO2,AIO4        A(2K IO AREA)                                
         LA    RE,FETCHWRK                                                      
         STCM  RE,15,RFTAWRK       A(6K WORK AREA)                              
         MVC   RFTCREP,REPALPHA    REP CODE                                     
         MVC   RFTCSRC,RTSRVC      RATING SERVICE                               
         MVI   RFTAMODE,RFTAMSTQ   FETCH MODE                                   
         MVI   RFTCDCTL,RFTCDC1Q   FETCH METHOD                                 
         LA    RE,FTCHHOOK         HOOK ROUTINE                                 
         STCM  RE,15,RFTHOOKA                                                   
         OI    RFTCNTL,RFTCHDRQ    INCLUDE HEADER                               
         TM    MISCFLG1,MF1TXT     TEXT ONLY REQUEST?                           
         BZ    *+16                NO                                           
         OI    RFTCNTL,RFTCTXTQ    INCLUDE TEXT                                 
         MVI   RFTCTXTT,RFTCTXIQ   FROM INVENTORY                               
         B     XPSTA010                                                         
*                                                                               
         OI    RFTCNTL,RFTCDEMQ    INCLUDE DEMOS                                
         OI    RFTCNTL,RFTCSLVQ    INCLUDE SHARES AND LEVELS                    
         OI    RFTCNTL,RFTCFTNQ    INCLUDE FOOTNOTES                            
         OI    RFTCNTL,RFTCRTEQ    INCLUDE RATES                                
         OI    RFTCNTL,RFTCRNWQ    USE NEW STYLE RATES                          
*                                                                               
XPSTA010 DS    0H                                                               
         MVI   RFTCTXTW,FTCHWDTH   WIDTH OF FETCH RETURN                        
*                                                                               
         L     RE,OVPARMS+4        POINT TO CURRENT STATIONS                    
         A     RE,CURSTA                                                        
         MVC   RFTCSTAT,0(RE)      STATION CALL LETTERS                         
         CLI   RFTCSTAT+4,C' '     NEED 'T' SET?                                
         BNE   *+8                 NO                                           
         MVI   RFTCSTAT+4,C'T'                                                  
*                                                                               
         L     RE,OVPARMS+4        POINT TO CURRENT INVENOTRY                   
         A     RE,CURINV                                                        
         MVC   RFTCINV,0(RE)       INVENTORY NUMBER                             
         CLC   RFTCINV,SPACES      ANY INVENTORY NUMBER?                        
         BL    XPSTA100            NO SKIP PLACE HOLDER                         
*                                                                               
         LA    R0,L'RFTCINV(RE)                                                 
*                                  EFFECTIVE DATES                              
         GOTO1 VDATCON,DMCB,(8,(R0)),(2,RFTCEFST)                               
         AH    R0,=Y(L'FLTSTART)                                                
         LR    RE,R0                                                            
*                                                                               
         OC    0(3,RE),0(RE)                                                    
         BZ    XPSTA020                                                         
         GOTO1 VDATCON,DMCB,(8,(R0)),(2,RFTCEFEN)                               
*                                                                               
XPSTA020 DS    0H                                                               
         L     RF,AIO1                                                          
         ST    RF,ACURPARM         STORE ADDRESS OF CURRENT PARMS               
         EJECT                                                                  
*-----------------*                                                             
* BUILD BOOK LIST *                                                             
*-----------------*                                                             
         CLI   NUMDEMS,0                                                        
         BE    DEMSX99                                                          
*                                                                               
         CLI   NUMBKS,0                                                         
         BE    BKSX099                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         MVI   RFTCBKS,LONGPARM    SET PARM AS NULL TERM. LIST                  
         STCM  RF,15,RFTCBKS+1                                                  
         ZIC   R1,NUMBKS           SET NUMBER OF REMAINING BOOKS                
         L     RE,OVPARMS+4        POINT TO CURRENT BOOK                        
         A     RE,FRSTBK                                                        
BKSX010  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCBKLQ*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCBKS,RF                                                       
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         MVC   0(RFTCBKLQ,RF),0(RE)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         LA    RE,BKLENQ(RE)                                                    
         DROP  RF                                                               
*                                                                               
         BCT   R1,BKSX010                                                       
*                                                                               
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         ST    RF,ACURPARM                                                      
*                                                                               
BKSX099  DS    0H                                                               
         EJECT                                                                  
*--------------------*                                                          
* BUILD UPGRADE LIST *                                                          
*--------------------*                                                          
         CLI   NUMUPGS,0                                                        
         BE    UPGSX99                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         STCM  RF,15,RFTCUPGA                                                   
         ZIC   R1,NUMUPGS          SET NUMBER OF REMAINING BOOKS                
         L     RE,OVPARMS+4        POINT TO CURRENT BOOK                        
         A     RE,FRSTUPG                                                       
UPGSX10  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(UPGLENQ*2))                                         
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         MVC   0(UPGLENQ-1,RF),0(RE)                                            
         LA    RF,UPGLENQ-1(RF)                                                 
         LA    RE,UPGLENQ(RE)                                                   
         BCT   R1,UPGSX10                                                       
*                                                                               
         XC    0(UPGLENQ,RF),0(RF)                                              
         LA    RF,UPGLENQ(RF)                                                   
         ST    RF,ACURPARM                                                      
*                                                                               
UPGSX99  DS    0H                                                               
         EJECT                                                                  
*-----------------*                                                             
* BUILD DEMO LIST *                                                             
*-----------------*                                                             
         LA    RF,RFTCDEMS                                                      
         ZIC   R1,NUMDEMS          SET NUMBER OF REMAINING DEMOS                
         CLI   NUMDEMS,((RFTCBKS-RFTCDEMS)/3)-1                                 
         BNH   *+8                 NOT TOO MANY DEMOS                           
         MVI   R1,((RFTCBKS-RFTCDEMS)/3)-1                                      
         L     RE,OVPARMS+4        POINT TO CURRENT DEMO                        
         A     RE,FRSTDEM                                                       
DEMSX10  DS    0H                  LOOP AND SET DEMOS                           
         MVC   0(3,RF),0(RE)                                                    
         LA    RF,3(RF)                                                         
         LA    RE,DEMLENQ(RE)                                                   
         BCT   R1,DEMSX10                                                       
*                                                                               
DEMSX99  DS    0H                                                               
         EJECT                                                                  
*----------------------*                                                        
* BUILD RATE CARD LIST *                                                        
*----------------------*                                                        
RCDSX00  DS    0H                  LOOP AND SET RATE CARDS                      
         CLI   NUMRCDS,0                                                        
         BE    RCDSX99                                                          
*                                                                               
         MVC   RFTCRDDT(3),FLTSTART                                             
         MVC   RFTCRDDT+3(3),FLTEND                                             
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         STCM  RF,15,RFTCRDRC                                                   
         ZIC   R1,NUMRCDS          SET NUMBER OF REMAINING RATE CARDS           
         L     RE,OVPARMS+4        POINT TO CURRENT RATE CARD                   
         A     RE,FRSTRCD                                                       
RCDSX10  DS    0H                  LOOP AND SET RATES                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCRTSL*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCRTES,RF                                                      
         XC    0(RCDLENQ,RF),0(RF)                                              
         MVC   0(RCDLENQ,RF),0(RE)                                              
         LA    RF,RCDLENQ(RF)                                                   
         LA    RE,RCDLENQ(RE)                                                   
         DROP  RF                                                               
*                                                                               
         BCT   R1,RCDSX10                                                       
*                                                                               
         XC    0(RCDLENQ,RF),0(RF)                                              
         LA    RF,RCDLENQ(RF)                                                   
         ST    RF,ACURPARM                                                      
*                                                                               
RCDSX99  DS    0H                                                               
         EJECT                                                                  
*------------*                                                                  
* FETCH CALL *                                                                  
*------------*                                                                  
         CLI   MODE,C'D'                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    MISCFLG1,FF-MF1TMPB2   SET NEW INVENTORY                         
         GOTO1 VFETCH,DMCB,FETCHBLK                                             
         DROP  R4                                                               
*                                                                               
XPSTA100 DS    0H                  PROCESS NEXT INVENTORY                       
         L     RE,OVPARMS+4                                                     
         A     RE,CURINV                                                        
         LA    RE,10(RE)                                                        
         CLI   0(RE),0             END OF INVENOTRY FOR THIS STATION?           
         BNE   XPSTA110            YES                                          
*                                                                               
         LA    RE,1(RE)            PROCESS NEXT STATION                         
         CLI   0(RE),0             END OF STATIONS?                             
         BE    XPSTAX              YES                                          
*                                                                               
         LR    RF,RE                                                            
         S     RF,OVPARMS+4                                                     
         ST    RF,CURSTA                                                        
*DHAB    NI    MISCFLG1,FF-MF1TMPB1   SET NEW STATION                           
*                                                                               
         LA    RE,5(RE)            BUMP TO INVENTORY                            
*DHAB    ZAP   INVSEQ,=P'0'                                                     
*                                                                               
XPSTA110 DS    0H                  PROCESS NEXT INVENTORY                       
         S     RE,OVPARMS+4                                                     
         ST    RE,CURINV                                                        
         AP    INVSEQ,=P'1'                                                     
         B     XPSTA000               FETCH IT                                  
*                                                                               
XPSTAX   DS    0H                  END OF FETCH LOOP                            
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HOOK FOR THE FETCH ROUTINE TO ADD NEW DETAIL CLUSTERS                         
***********************************************************************         
FTCHHOOK NTR1  BASE=*,LABEL=*                                                   
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RFTMODE,RFTNHDRQ                                                 
         BE    INVHDR                                                           
         CLI   RFTMODE,RFTNBKQ                                                  
         BE    INVBK                                                            
         CLI   RFTMODE,RFTNRTEQ                                                 
         BE    INVRCD                                                           
         CLI   RFTMODE,RFTNTXTQ                                                 
         BE    INVTXT                                                           
         B     EXITOK                                                           
         EJECT                                                                  
*===============================*                                               
* PROCESS INVENTORY HEADER HOOK *                                               
*===============================*                                               
INVHDR   DS    0H                                                               
         OC    RFTFPGMS,RFTFPGMS                                                
         BZ    IHDRNO              NOT GOOD!!!                                  
*                                                                               
         CLI   MODE,C'D'           NEWDATA CALL?                                
         BE    IHDR0020            YES - SKIP FLIGHT CHECK                      
*                                                                               
         XC    WORK,WORK           CONVERT DATES                                
         GOTO1 VDATCON,DMCB,(2,RFTFEFST),(19,WORK)                              
         OC    RFTFEFEN,RFTFEFEN                                                
         BZ    IHDR0002                                                         
         GOTO1 VDATCON,DMCB,(2,RFTFEFEN),(19,WORK+3)                            
*                                                                               
IHDR0002 DS    0H                                                               
         CLI   NUMFLTS,1           SINGLE FLIGHT?                               
         BE    IHDR0020            YES- NO NEED TO CHECK FLIGHTS                
*                                                                               
         ZIC   RF,NUMFLTS          CHECK EFFECTIVE DATES AGAINST ALL            
         L     RE,FRSTFLT           REQUEST FLIGHTS                             
         A     RE,OVPARMS+4                                                     
IHDR0010 DS    0H                                                               
         CLC   WORK(3),3(RE)       INVENTORY STARTS AFTER FILTER END?           
         BH    IHDR0014            YES- FILTER FAILED                           
*                                                                               
         OC    WORK+3(3),WORK+3    OPEN ENDED INVENTORY?                        
         BZ    IHDR0020            YES                                          
         CLC   WORK+3,0(RE)        INVENTORY ENDS B4 FILTER START?              
         BNH   IHDR0014            YES - FILTER FAILED                          
*                                                                               
         B     IHDR0020            FILTER PASSED                                
*                                                                               
IHDR0014 DS    0H                                                               
         LA    RE,FLTLENQ(RE)      CHECK NEXT FLIGHT                            
         BCT   RF,IHDR0010                                                      
         B     IHDRNO                                                           
*                                                                               
IHDR0020 DS    0H                  PROCESS HEADER                               
         TM    MISCFLG1,MF1TMPB1   NEED STATION HEADER?                         
         BNZ   IHDR0030            NO                                           
*                                  ADD NEW STATION ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,ISTDATA,0                                   
*                                  STATION CALL LETTERS                         
         MVC   WORK+20(5),RFTCSTAT                                              
         CLI   WORK+24,C'T'                                                     
         BNE   *+8                                                              
         MVI   WORK+24,C' '                                                     
*                                                                               
         LA    R1,WORK+20          SWITCH HISPANIC CALL LETTERS BACK            
         BAS   RE,SWHISP                                                        
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,ISTSTAEL,WORK+20,0                          
*                                                                               
         OI    MISCFLG1,MF1TMPB1   SET STATION INDICATED                        
IHDR0030 DS    0H                                                               
         CLI   MODE,C'D'           NEWDATA CALL?                                
         BE    IHDRYES             YES                                          
*                                                                               
IHDR0040 DS    0H                                                               
*                                  ADD NEW INVENTORY ELEMENT                    
         GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                   INVENTORY NUMBER                            
         GOTO1 AADDDATA,DMCB,AFABLK,INVNUMEL,RFTFINV,0                          
*                                   EFFECTIVE START DATE                        
         GOTO1 AADDDATA,DMCB,AFABLK,INVESTEL,WORK,0                             
*                                   EFFECTIVE END DATE                          
         OC    RFTFEFEN,RFTFEFEN                                                
         BZ    IHDR0044                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,INVEENEL,WORK+3,0                           
*                                                                               
IHDR0044 DS    0H                                                               
         SPACE 2                                                                
*----------------*                                                              
* HEADER DAYPART *                                                              
*----------------*                                                              
         LA    R0,RFTFDTM           DEFAULT IS MATCHED DAYPART                  
         TM    SELPROF+SELPDPB,SELPDPA                                          
         BNO   IHDDP020             USE MATCHED DAYPART                         
*                                                                               
         ZIC   RF,NUMDPTS          CHECK FOR PRINCIPLE DAYPART                  
         L     RE,FRSTDPT                                                       
         A     RE,OVPARMS+4                                                     
IHDDP010 CLC   0(1,RE),RFTFDPTS    MATCH ON PRINCIPLE DAYPART?                  
         BNE   *+12                NO                                           
         LA    R0,0(RE)                                                         
         B     IHDDP020                                                         
*                                                                               
         LA    RE,DPTLENQ(RE)                                                   
         BCT   RF,IHDDP010                                                      
*                                                                               
IHDDP020 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,INVDPTEL,(R0),0                             
         SPACE 2                                                                
*------------------*                                                            
* HEADER DAY/TIMES *                                                            
*------------------*                                                            
*                                  DAYS                                         
         GOTO1 AADDDATA,DMCB,AFABLK,INVDAYEL,RFTFDTDY,0                         
*                                  START TIME                                   
         GOTO1 AADDDATA,DMCB,AFABLK,INVSTMEL,RFTFDTST,0                         
*                                  END TIME                                     
         LA    R0,RFTFDTEN                                                      
         CLC   RFTFDTEN,=C'CC'                                                  
         BNE   *+8                                                              
         LA    R0,=X'00C8'                                                      
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,INVETMEL,(R0),0                             
         SPACE 2                                                                
*-----------------------------------*                                           
* HEADER OVERRIDE (AVAIL) DAY/TIMES *                                           
*-----------------------------------*                                           
         LA    R0,8                                                             
         LA    R6,RFTFAVLS                                                      
IHDAV010 DS    0H                                                               
         CLC   0(L'RFTFAVLS,R6),SPACES                                          
         BH    *+12                                                             
         LA    R6,(L'RFTFAVLS/2)(R6)                                            
         B     IHDAV020                                                         
*                                                                               
         OC    0(L'RFTFAVLS,R6),SPACES                                          
*                                  1/2 THE FIELD IS DAY                         
         LA    RF,(L'RFTFAVLS/2)-1(R6)                                          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,R6                                                            
         LA    RF,1(RF)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,INVAVDEL,(R6),(RF)                          
*                                                                               
*                                  THE OTHER 1/2 IS TIME                        
         LA    R6,(L'RFTFAVLS/2)(R6)                                            
         LA    RF,(L'RFTFAVLS/2)-1(R6)                                          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,R6                                                            
         LA    RF,1(RF)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,INVAVTEL,(R6),(RF)                          
*                                                                               
IHDAV020 DS    0H                                                               
         LA    R6,(L'RFTFAVLS/2)(R6)                                            
         BCT   R0,IHDAV010                                                      
         EJECT                                                                  
*----------------------*                                                        
* HEADER PROGRAM NAMES *                                                        
*----------------------*                                                        
         LA    R0,8                                                             
         LA    R6,RFTFPGMS                                                      
IHDPR010 DS    0H                                                               
         CLC   0(L'RFTFPGMS,R6),SPACES                                          
         BNH   IHDPR020                                                         
*                                                                               
         LA    RF,L'RFTFPGMS-1(R6)                                              
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
*                                                                               
         SR    RF,R6                                                            
         LA    RF,1(RF)                                                         
         OC    0(L'RFTFPGMS,R6),SPACES                                          
         GOTO1 AADDDATA,DMCB,AFABLK,INVPRGEL,(R6),(RF)                          
*                                                                               
IHDPR020 DS    0H                                                               
         LA    R6,L'RFTFPGMS(R6)                                                
         BCT   R0,IHDPR010                                                      
*                                                                               
IHDRYES  DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
IHDRNO   DS    0H                                                               
         MVI   RFTRETRN,INVREJ     SET FAILED APPLICATION FILTER                
         B     EXITOK                                                           
         EJECT                                                                  
*=============================*                                                 
* PROCESS INVENTORY BOOK HOOK *                                                 
*=============================*                                                 
INVBK    DS    0H                                                               
         OC    RFTFDEMS(24*4),RFTFDEMS                                          
         BNZ   IBK00001                                                         
         OC    RFTFSHRS(24*4),RFTFSHRS                                          
         BNZ   IBK00001                                                         
         OC    RFTFLVLS(24*4),RFTFLVLS                                          
         BZ    EXITOK              NO INTERESTING DATA                          
*                                                                               
IBK00001 DS    0H                                                               
         MVI   NEWBOOK,C'Y'                                                     
*                                                                               
         OC    RFTFBK,RFTFBK       WAS IT A BOOK?                               
         BZ    INVUPG              NO - ITS AN UPGRADE                          
*                                                                               
         CLI   MODE,C'D'           NEWDATA CALL?                                
         BNE   IBK00002            NO                                           
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY HEADER?                       
         BNZ   IBK00002            NO                                           
*                                  ADD NEW INVENTORY ELEMENT                    
         GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
*                                  INVENTORY SEQUENCE NUMBER                    
         GOTO1 AADDDATA,DMCB,AFABLK,INVSEQEL,INVSEQ,0                           
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IBK00002 DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
         ZIC   RF,NUMBKS           GET BOOK SEQUENCE NUMBER                     
         L     RE,FRSTBK                                                        
         A     RE,OVPARMS+4                                                     
IBK0010  DS    0H                                                               
         CLC   RFTFBK,0(RE)        BOOK MATCH?                                  
         BE    IBK0020             YES                                          
         LA    RE,BKLENQ(RE)                                                    
         BCT   RF,IBK0010                                                       
         DC    H'0'                UM - THIS CAN'T HAPPEN                       
*                                                                               
IBK0020  DS    0H                                                               
         TM    L'RFTFBK(RE),X'80'  OLD BOOK?                                    
         BNO   *+8                 NO                                           
         MVI   NEWBOOK,C'N'                                                     
*                                                                               
         ZIC   RE,NUMBKS                                                        
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
*                                                                               
*                                  ADD NEW BOOK ELEMENT                         
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
*                                  SET SEQUENCE NUMBER                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSSEQEL,BYTE,0                             
*                                                                               
         L     RE,RFTFTX1A         GET FOOTNOTE LENGTH                          
         OC    0(30,RE),SPACES                                                  
         LA    RE,30(RE)                                                        
IBK0030  CLI   0(RE),C' '                                                       
         BH    IBK0032                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   IBK0030                                                          
         B     IBK0040             NO SIGNIFICANT FOOTNOTE                      
*                                                                               
IBK0032  DS    0H                                                               
         S     RE,RFTFTX1A                                                      
         LA    R0,1(RE)            SAVE FOOTNOTE LENGTH                         
*                                                                               
*&&DO                                                                           
         MVI   BYTE,3              SET FLUFF LENGTH                             
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFOOEL,BYTE,0                             
*&&                                SET FOOTNOTE                                 
         L     RF,RFTFTX1A                                                      
         LA    RF,3(RF)            SKIP FLUFF FOR NOW                           
         SH    R0,=H'3'                                                         
         BNP   IBK0040                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFTNEL,(RF),(R0)                          
*                                                                               
IBK0040  DS    0H                  DO DEMOS                                     
         B     INVDEM                                                           
*================================*                                              
* PROCESS INVENTORY UPGRADE HOOK *                                              
*================================*                                              
INVUPG   DS    0H                                                               
         CLI   MODE,C'D'           NEWDATA CALL?                                
         BNE   IUPG002             NO                                           
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY HEADER?                       
         BNZ   IUPG002             NO                                           
*                                  ADD NEW INVENTORY ELEMENT                    
         GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
*                                  INVENTORY SEQUENCE NUMBER                    
         GOTO1 AADDDATA,DMCB,AFABLK,INVSEQEL,INVSEQ,0                           
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IUPG002  DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
*                                  SET SEQUENCE NUMBER                          
         SR    R0,R0                                                            
         L     R1,RFTFUPGA                                                      
         S     R1,RFTCUPGA         DISPLACEMENT TO UPGRADE                      
         LTR   R1,R1                                                            
         BZ    IUPG010                                                          
*                                                                               
         LA    RE,UPGLENQ-1                                                     
         DR    R0,RE                                                            
IUPG010  DS    0H                                                               
         LA    R0,1(R1)            1 BASED SEQUENCE NUMBER                      
         STC   R0,BYTE                                                          
*                                                                               
         MHI   R1,UPGLENQ          ADDRESS INPUT UPGRADE                        
         A     R1,FRSTUPG                                                       
         A     R1,OVPARMS+4                                                     
         TM    UPGLENQ-1(R1),X'80'  OLD BOOK?                                   
         BNO   *+8                  NO                                          
         MVI   NEWBOOK,C'N'                                                     
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSUPGEL,BYTE,0                             
*                                                                               
IUPG040  DS    0H                                                               
         L     RE,RFTFTX1A         GET FOOTNOTE LENGTH                          
         OC    0(30,RE),SPACES                                                  
         LA    RE,30(RE)                                                        
IUPG050  CLI   0(RE),C' '                                                       
         BH    IUPG052                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   IUPG050                                                          
         B     IUPG060             NO SIGNIFICANT FOOTNOTE                      
*                                                                               
IUPG052  DS    0H                                                               
         S     RE,RFTFTX1A                                                      
         LA    R0,1(RE)            SAVE FOOTNOTE LENGTH                         
*                                                                               
*&&DO                                                                           
         MVI   BYTE,3              SET FLUFF LENGTH                             
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFOOEL,BYTE,0                             
*&&                                SET FOOTNOTE                                 
         L     RF,RFTFTX1A                                                      
         LA    RF,3(RF)            SKIP FLUFF FOR NOW                           
         SH    R0,=H'3'                                                         
         BNP   IUPG060                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFTNEL,(RF),(R0)                          
*                                                                               
IUPG060  DS    0H                  DO DEMOS                                     
         B     INVDEM                                                           
         EJECT                                                                  
*========================*                                                      
* PROCESS INVENTORY DEMO *                                                      
*========================*                                                      
INVDEM   DS    0H                                                               
         LA    R5,1                DEMO SEQUENCE NUMBER                         
         LA    R2,RFTFDEMS         RATING VALUES                                
         LA    R3,RFTFSHRS         SHARE VALUES                                 
         LA    R6,RFTFLVLS         HUT/PUT LEVEL VALUES                         
*                                                                               
IDEM010  DS    0H                                                               
         OC    0(4,R2),0(R2)       ANY RATING?                                  
         BNZ   IDEM020             YES                                          
         OC    0(4,R3),0(R3)       ANY SHARE?                                   
         BNZ   IDEM020             YES                                          
         OC    0(4,R6),0(R6)       ANY HUT/PUT LEVEL?                           
         BNZ   IDEM020             YES                                          
         B     IDEM050             DON'T SEND THIS DEMO                         
*                                                                               
IDEM020  DS    0H                                                               
         CLI   NEWBOOK,C'Y'        NEW BOOK?                                    
         BE    IDEM022             YES - SHOW ALL DEMOS                         
*                                                                               
         LR    R1,R5               ADDRESS INPUT DEMO                           
         BCTR  R1,0                                                             
         MHI   R1,DEMLENQ                                                       
         A     R1,FRSTDEM                                                       
         A     R1,OVPARMS+4                                                     
         TM    3(R1),X'80'         OLD DEMO?                                    
         BO    IDEM050             YES - SKIP FOR OLD BOOK                      
*                                                                               
IDEM022  DS    0H                                                               
*                                  ADD NEW DEMO ELEMENT                         
         GOTO1 ASETELEM,DMCB,AFABLK,DEMDATA,0                                   
*                                                                               
         STC   R5,BYTE             SET SEQUENCE NUMBER                          
         GOTO1 AADDDATA,DMCB,AFABLK,DEMSEQEL,BYTE,0                             
*                                                                               
         OC    0(4,R2),0(R2)       ANY RATING?                                  
         BZ    IDEM030             NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,DEMRTGEL,(R2),0                             
*                                                                               
IDEM030  DS    0H                                                               
         OC    0(4,R3),0(R3)       ANY SHARE?                                   
         BZ    IDEM040             NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,DEMSHREL,(R3),0                             
*                                                                               
IDEM040  DS    0H                                                               
         OC    0(4,R6),0(R6)       ANY HUT/PUT LEVEL?                           
         BZ    IDEM050             NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,DEMLVLEL,(R6),0                             
*                                                                               
IDEM050  DS    0H                                                               
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R6,4(R6)                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         CLM   R5,1,NUMDEMS                                                     
         BNH   IDEM010                                                          
*                                                                               
         B     EXITOK                                                           
*==================================*                                            
* PROCESS INVENTORY RATE CARD HOOK *                                            
*==================================*                                            
INVRCD   DS    0H                                                               
*                                                                               
         LA    R3,RFTFRDWK         CHECK FOR ANY NON ZERO RATE                  
R        USING RFTFRDWK,R3                                                      
         LA    RF,RFTFRDEN                                                      
         OC    R.RFTFRDRT,R.RFTFRDRT                                            
         BNZ   *+16                                                             
         LA    R3,RFTFRDSL(R3)                                                  
         BCT   RF,*-14                                                          
         B     INVRCDX                                                          
         DROP  R                                                                
*                                                                               
         CLI   MODE,C'D'           NEWDATA CALL?                                
         BNE   IRCD002             NO                                           
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY HEADER?                       
         BNZ   IRCD002             NO                                           
*                                  ADD NEW INVENTORY ELEMENT                    
         GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
*                                  INVENTORY SEQUENCE NUMBER                    
         GOTO1 AADDDATA,DMCB,AFABLK,INVSEQEL,INVSEQ,0                           
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IRCD002  DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,RCDDATA,0                                   
*                                  SET SEQUENCE NUMBER                          
         SR    R0,R0                                                            
         ICM   R1,15,RFTFRDRC                                                   
         ICM   RF,15,RFTCRDRC                                                   
         SR    R1,RF                                                            
         LTR   R1,R1                                                            
         BZ    IRCD010                                                          
*                                                                               
         LA    RE,RCDLENQ                                                       
         DR    R0,RE                                                            
IRCD010  DS    0H                                                               
         LA    R0,1(R1)            1 BASED SEQUENCE NUMBER                      
         STC   R0,BYTE                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCDSEQEL,BYTE,0                             
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCD1WKEL,RFTFRDWK,0                         
*                                  SET 1ST WEEK DATE                            
         GOTO1 AADDDATA,DMCB,AFABLK,RCDRATEL,RFTFRDRT,0                         
*                                  SET RATE                                     
         ICM   R2,15,RFTFRDRT                                                   
         LA    R3,RFTFRDWK                                                      
R        USING RFTFRDWK,R3                                                      
         LA    R5,RFTFRDEN-1                                                    
IRCD0020 DS    0H                                                               
         OC    RFTFRDSL(L'RFTFRDWK,R3),RFTFRDSL(R3)                             
         BZ    IRCD0040             NEXT WEEK IS OMMITTED. EXIT                 
*                                                                               
         LA    R3,RFTFRDSL(R3)                                                  
         CLM   R2,15,R.RFTFRDRT      NEW RATE?                                  
         BE    IRCD0030            NO                                           
*                                                                               
         ICM   R2,15,R.RFTFRDRT                                                 
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RCD1WKEL,R.RFTFRDWK,0                       
*                                  SET 1ST WEEK DATE                            
         GOTO1 AADDDATA,DMCB,AFABLK,RCDRATEL,R.RFTFRDRT,0                       
*                                  SET RATE                                     
IRCD0030 DS    0H                                                               
         BCT   R5,IRCD0020                                                      
*                                                                               
IRCD0040 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(8,R.RFTFRDWK),(0,WORK)                             
         GOTO1 VADDAY,DMCB,WORK,WORK,6                                          
         GOTO1 VDATCON,DMCB,(0,WORK),(19,WORK+6)                                
         GOTO1 AADDDATA,DMCB,AFABLK,RCDLWKEL,WORK+6,0                           
*                                  SET LAST WEEK DATE                           
         DROP  R                                                                
INVRCDX  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*=============================*                                                 
* PROCESS INVENTORY TEXT HOOK *                                                 
*=============================*                                                 
INVTXT   DS    0H                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY HEADER?                       
         BNZ   ITXT0010            NO                                           
*                                  ADD NEW INVENTORY ELEMENT                    
         GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
*                                  INVENTORY SEQUENCE NUMBER                    
         GOTO1 AADDDATA,DMCB,AFABLK,INVSEQEL,INVSEQ,0                           
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
ITXT0010 DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,TXTDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,TXTNUMEL,RFTFTXT#,0                         
*                                                                               
         CLI   RFTFTXFL,C'Y'       WRAPPING TEXT?                               
         BNE   ITXT0012            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,TXTWRPEL,0,0                                
*                                                                               
ITXT0012 DS    0H                                                               
*                                                                               
         L     R3,RFTFTXTA         A(FILTERS)                                   
         SR    R0,R0                                                            
         ICM   R0,1,RFTFTXTN       LINE COUNT                                   
         BZ    ITXT0100                                                         
*                                  SPACE FILL BUFFER                            
         OC    0(132,R3),SPACES                                                 
         LA    R3,132(R3)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         ZIC   R3,RFTFTX1N         TEXT LINE COUNT                              
         ZIC   R0,RFTFTXTN         TEXT + FILTER LINE COUNT                     
         SR    R0,R3               FILTER LINE COUNT                            
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BNP   ITXT0038                                                         
*                                                                               
         L     R3,RFTFTXTA         A(FILTERS)                                   
ITXT0030 DS    0H                                                               
         CLC   0(132,R3),SPACES    SEND SINGLE SPACE IF WHOLE LINE              
         BNE   *+12                                                             
         LA    RF,1                                                             
         B     ITXT0032                                                         
*                                                                               
         LA    RF,131(R3)          REMOVE TRAILING SPACES                       
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         SR    RF,R3                                                            
         LA    RF,1(RF)            LENGTH W/0 SPACES                            
*                                                                               
ITXT0032 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,TXTFLTEL,(R3),(RF)                          
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,ITXT0030                                                      
*                                                                               
ITXT0038 DS    0H                                                               
         L     R3,RFTFTX1A         A(FIRST LINE)                                
         ZIC   R0,RFTFTX1N         LINE COUNT                                   
*                                                                               
         LTR   R0,R0               IDIOTS THAT PUT IN NO TEXT                   
         BZ    ITXT0100                                                         
*                                                                               
ITXT0040 DS    0H                                                               
         CLC   0(132,R3),SPACES    SEND SINGLE SPACE IF WHOLE LINE              
         BNE   *+12                                                             
         LA    RF,1                                                             
         B     ITXT0042                                                         
*                                                                               
         LA    RF,131(R3)          REMOVE TRAILING SPACES                       
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         SR    RF,R3                                                            
         LA    RF,1(RF)            LENGTH W/0 SPACES                            
ITXT0042 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,TXTTXTEL,(R3),(RF)                          
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,ITXT0040                                                      
*                                                                               
ITXT0100 DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*********************************************************************           
NEWTEXT  NTR1  BASE=*,LABEL=*                                                   
         MVI   MODE,C'T'                                                        
         L     R2,OVPARMS+4                                                     
*                                                                               
         MVC   NUMSTAS,0(R2)       NUMBER OF STATIONS                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST STATION                         
         ST    RF,FRSTSTA                                                       
         ZIC   RF,NUMSTAS                                                       
         MH    RF,=Y(STALENQ)      BUMP TO BOOKS                                
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMBKS,0(R2)        NUMBER OF BOOKS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST BOOK                            
         ST    RF,FRSTBK                                                        
         ZIC   RF,NUMBKS                                                        
         MH    RF,=Y(BKLENQ)       BUMP TO DEMOS                                
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMDEMS,0(R2)       NUMBER OF DEMOS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST DEMO                            
         ST    RF,FRSTDEM                                                       
         ZIC   RF,NUMDEMS                                                       
         MH    RF,=Y(DEMLENQ)      BUMP TO END                                  
         AR    R2,RF                                                            
*                                                                               
         MVC   CURSTA,FRSTSTA      SET CURRENT STATION                          
         MVC   REMSTAS,NUMSTAS     SET NUMBER OF REMAINING STATIONS             
*                                                                               
         CLI   NUMSTAS,0                                                        
         BE    EXITOK                                                           
*                                                                               
         NI    MISCFLG1,FF-MF1TMPB1   SET NEW STATION                           
********************************************************                        
* LOOP THROUGH ALL THE STATIONS                                                 
********************************************************                        
NTEXT000 DS    0H                                                               
         LA    R0,FETCHBLK         CLEAR THE FETCH PARAMTER BLOCK               
         LH    R1,=Y(RFTBLKL)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOMFACS    A(COMFACS)                                   
         MVC   RFTAIO1,AIO3        A(2K IO AREA)                                
         MVC   RFTAIO2,AIO4        A(2K IO AREA)                                
         LA    RE,FETCHWRK                                                      
         STCM  RE,15,RFTAWRK       A(6K WORK AREA)                              
         MVC   RFTCREP,REPALPHA    REP CODE                                     
         MVI   RFTAMODE,RFTATXTQ   FETCH TEXT                                   
         MVC   RFTCSRC,RTSRVC      RATING SERVICE                               
         LA    RE,TEXTHOOK         HOOK ROUTINE                                 
         STCM  RE,15,RFTHOOKA                                                   
         OI    RFTCNTL,RFTCTXTQ                                                 
         MVI   RFTCTXTW,FTCHWDTH   WIDTH OF FETCH RETURN                        
         MVI   RFTCTXTT,RFTCTXSQ   GET STATION TEXT                             
         TM    MISCFLG1,MF1MKT     MARKET TEXT REQUEST?                         
         BZ    *+8                                                              
         MVI   RFTCTXTT,RFTCTXMQ   YES                                          
*                                                                               
         L     RE,OVPARMS+4        POINT TO CURRENT STATIONS                    
         A     RE,CURSTA                                                        
         MVC   RFTCSTAT,0(RE)      STATION CALL LETTERS                         
         CLI   RFTCSTAT+4,C' '     NEED 'T' SET?                                
         BNE   *+8                 NO                                           
         MVI   RFTCSTAT+4,C'T'                                                  
*                                                                               
         L     RF,AIO1                                                          
         ST    RF,ACURPARM         STORE ADDRESS OF CURRENT PARMS               
         EJECT                                                                  
*-----------------*                                                             
* BUILD BOOK LIST *                                                             
*-----------------*                                                             
         CLI   NUMBKS,0                                                         
         BE    NTEXT30                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         MVI   RFTCBKS,LONGPARM    SET PARM AS NULL TERM. LIST                  
         STCM  RF,15,RFTCBKS+1                                                  
         ZIC   R1,NUMBKS           SET NUMBER OF REMAINING BOOKS                
         L     RE,OVPARMS+4        POINT TO CURRENT BOOK                        
         A     RE,FRSTBK                                                        
NTEXT10  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCBKLQ+1))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCBKS,RF                                                       
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         MVC   0(RFTCBKLQ,RF),0(RE)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         LA    RE,BKLENQ(RE)                                                    
         DROP  RF                                                               
*                                                                               
         BCT   R1,NTEXT10                                                       
*                                                                               
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         ST    RF,ACURPARM                                                      
*                                                                               
NTEXT30  DS    0H                                                               
         EJECT                                                                  
*-----------------*                                                             
* BUILD DEMO LIST *                                                             
*-----------------*                                                             
         CLI   NUMDEMS,0                                                        
         BE    NTEXT90                                                          
*                                                                               
         LA    RF,RFTCDEMS                                                      
         ZIC   R1,NUMDEMS          SET NUMBER OF REMAINING DEMOS                
         CLI   NUMDEMS,((RFTCBKS-RFTCDEMS)/3)-1                                 
         BNH   *+8                 NOT TOO MANY DEMOS                           
         MVI   R1,((RFTCBKS-RFTCDEMS)/3)-1                                      
         L     RE,OVPARMS+4        POINT TO CURRENT DEMO                        
         A     RE,FRSTDEM                                                       
NTEXT40  DS    0H                  LOOP AND SET DEMOS                           
         MVC   0(3,RF),0(RE)                                                    
         LA    RF,3(RF)                                                         
         LA    RE,DEMLENQ(RE)                                                   
         BCT   R1,NTEXT40                                                       
         EJECT                                                                  
*------------*                                                                  
* FETCH CALL *                                                                  
*------------*                                                                  
NTEXT90  DS    0H                                                               
         CLI   MODE,C'T'                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VFETCH,DMCB,FETCHBLK                                             
         DROP  R4                                                               
*                                                                               
NTEXT100 DS    0H                  PROCESS NEXT STATION                         
         ZIC   R0,REMSTAS          REMAINING STATIONS                           
         BCTR  R0,0                                                             
         STC   R0,REMSTAS                                                       
         CLI   REMSTAS,0           ANYMORE STATIONS?                            
         BNH   NTEXTX              NO                                           
*                                                                               
         L     RE,CURSTA           POINT TO CURRENT STATION                     
         LA    RE,5(RE)            BUMP TO NEXT STATION                         
         ST    RE,CURSTA                                                        
         NI    MISCFLG1,FF-MF1TMPB1   SET NEW STATION                           
         B     NTEXT000               FETCH IT                                  
*                                                                               
NTEXTX   DS    0H                  END OF FETCH LOOP                            
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HOOK FOR THE FETCH ROUTINE TO ADD MARKET/STATION TEXT                         
***********************************************************************         
TEXTHOOK NTR1  BASE=*,LABEL=*                                                   
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RFTMODE,RFTNTXTQ                                                 
         BE    MSTEXT                                                           
         B     EXITOK                                                           
         EJECT                                                                  
*=============================*                                                 
* PROCESS INVENTORY TEXT HOOK *                                                 
*=============================*                                                 
MSTEXT   DS    0H                                                               
         TM    MISCFLG1,MF1TMPB1   NEED STATION HEADER?                         
         BNZ   MSTEXT10            NO                                           
*                                  ADD NEW STATION ELEMENT                      
         GOTO1 ASETELEM,DMCB,AFABLK,ISTDATA,0                                   
*                                  STATION CALL LETTERS                         
         MVC   WORK+20(5),RFTCSTAT                                              
         CLI   WORK+24,C'T'                                                     
         BNE   *+8                                                              
         MVI   WORK+24,C' '                                                     
*                                                                               
         LA    R1,WORK+20          SWITCH HISPANIC CALL LETTERS BACK            
         BAS   RE,SWHISP                                                        
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,ISTSTAEL,WORK+20,0                          
*                                                                               
         OI    MISCFLG1,MF1TMPB1   SET STATION INDICATED                        
MSTEXT10 DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,TXTDATA,0                                   
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,TXTNUMEL,RFTFTXT#,0                         
*                                                                               
         CLI   RFTFTXFL,C'Y'       WRAPPING TEXT?                               
         BNE   MSTEXT12            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,TXTWRPEL,0,0                                
*                                                                               
MSTEXT12 DS    0H                                                               
*                                                                               
         L     R3,RFTFTXTA         A(FILTERS)                                   
         SR    R0,R0                                                            
         ICM   R0,1,RFTFTXTN       LINE COUNT                                   
         BZ    MSTEXT60                                                         
*                                  SPACE FILL BUFFER                            
         OC    0(132,R3),SPACES                                                 
         LA    R3,132(R3)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         ZIC   R3,RFTFTX1N         TEXT LINE COUNT                              
         ZIC   R0,RFTFTXTN         TEXT + FILTER LINE COUNT                     
         SR    R0,R3               FILTER LINE COUNT                            
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BNP   MSTEXT38                                                         
*                                                                               
         L     R3,RFTFTXTA         A(FILTERS)                                   
MSTEXT30 DS    0H                                                               
         CLC   0(132,R3),SPACES    SEND SINGLE SPACE IF WHOLE LINE              
         BNE   *+12                                                             
         LA    RF,1                                                             
         B     MSTEXT32                                                         
*                                                                               
         LA    RF,131(R3)          REMOVE TRAILING SPACES                       
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         SR    RF,R3                                                            
         LA    RF,1(RF)            LENGTH W/0 SPACES                            
MSTEXT32 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,TXTFLTEL,(R3),(RF)                          
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,MSTEXT30                                                      
*                                                                               
MSTEXT38 DS    0H                                                               
         L     R3,RFTFTX1A         A(FIRST LINE)                                
         ZIC   R0,RFTFTX1N         LINE COUNT                                   
*                                                                               
         LTR   R0,R0               IDIOTS THAT PUT IN NO TEXT                   
         BZ    MSTEXT60                                                         
*                                                                               
MSTEXT40 DS    0H                                                               
         CLC   0(132,R3),SPACES    SEND SINGLE SPACE IF WHOLE LINE              
         BNE   *+12                                                             
         LA    RF,1                                                             
         B     MSTEXT42                                                         
*                                                                               
         LA    RF,131(R3)          REMOVE TRAILING SPACES                       
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         SR    RF,R3                                                            
         LA    RF,1(RF)            LENGTH W/0 SPACES                            
MSTEXT42 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,TXTTXTEL,(R3),(RF)                          
*                                                                               
         LA    R3,132(R3)                                                       
         BCT   R0,MSTEXT40                                                      
*                                                                               
MSTEXT60 DS    0H                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
ACURPARM DS    F                                                                
*                                                                               
FRSTSTA  DS    F                                                                
CURSTA   DS    F                                                                
FRSTBK   DS    F                                                                
CURBK    DS    F                                                                
FRSTUPG  DS    F                                                                
CURUPG   DS    F                                                                
FRSTDEM  DS    F                                                                
CURDEM   DS    F                                                                
FRSTDPT  DS    F                                                                
CURDPT   DS    F                                                                
FRSTRCD  DS    F                                                                
CURRCD   DS    F                                                                
FRSTFLT  DS    F                                                                
CURFLT   DS    F                                                                
*                                                                               
CURINV   DS    F                                                                
*                                                                               
NUMSTAS  DS    X                                                                
NUMDEMS  DS    X                                                                
NUMRCDS  DS    X                                                                
NUMFLTS  DS    X                                                                
NUMDPTS  DS    X                                                                
NUMBKS   DS    X                                                                
NUMUPGS  DS    X                                                                
*                                                                               
REMSTAS  DS    X                                                                
REMDEMS  DS    X                                                                
REMRCDS  DS    X                                                                
REMFLTS  DS    X                                                                
REMDPTS  DS    X                                                                
REMBKS   DS    X                                                                
REMUPGS  DS    X                                                                
*                                                                               
FLTSTART DS    XL3                                                              
FLTEND   DS    XL3                                                              
*                                                                               
MODE     DS    C                                                                
*                                                                               
NEWBOOK  DS    C                                                                
*                                                                               
INVSEQ   DS    PL5                                                              
*                                                                               
FETCHBLK DS    CL(RFTBLKL)         FETCH BLOCK                                  
*                                                                               
FETCHWRK DS    XL6144                                                           
OVERWRKQ EQU   *-OVERWRKD          LENGTH OF WORKING STORAGE                    
         EJECT                                                                  
       ++INCLUDE REFETCHD                                                       
       ++INCLUDE REPRPWORKD                                                     
MAPTABD  DSECT                                                                  
       ++INCLUDE REPRPMAP                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019REPRX20S  11/01/99'                                      
         END                                                                    
*                                                                               
