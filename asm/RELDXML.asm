*          DATA SET RELDXML    AT LEVEL 092 AS OF 09/16/05                      
*PHASE RELDXML                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'RELDMCI - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
**********************************************************************          
*  16SEP05 (KUI) --- FIX MISDIRECTED XML ORDERS                                 
*                                                                    *          
*                                                                    *          
**********************************************************************          
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
*******************************************************************             
*                                                                 *             
*  DARE RECORD PURGE                                              *             
*                                                                 *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
*        AP    10(5,R5),=P'1'                                                   
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         MVC   P(08),=C'STARTED!'                                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RDARREC,R5                                                       
         CLC   =X'4100',RDARKTYP                                                
         BE    DMXREC10                                                         
         CLC   =X'5100',RDARKTYP                                                
         BE    DMXREC10                                                         
         B     DMXKEEP                                                          
         DROP  R5                                                               
*                                                                               
DMXREC10 DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RDARREC,R5                                                       
         CLC   =C'BL',RDARKREP                                                  
         BE    DMXREC20                                                         
         CLC   =C'PV',RDARKREP                                                  
         BNE   DMXKEEP                                                          
DMXREC20 DS    0H                                                               
         CLI   RDARKRT,X'10'       HEADER RECORDS ONLY                          
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   =C'ITNNY',RDARKAGY                                               
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   =C'BO',RDARRCVR+3                                                
         BNE   DMXKEEP                                                          
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         MVI   ELCODE,X'0F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
         USING RDARFLEM,R5                                                      
         TM    RDARFLG2,X'80'      XML?                                         
         BZ    DMXKEEP                                                          
         DROP  R5                                                               
*                                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RDARREC,R5                                                       
*                                                                               
         MVC   P(12),=C'** BEFORE **'                                           
         MVC   P+14(80),0(R5)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   RDARRCVR+3(2),=C'NY'                                             
*                                                                               
         MVC   P(11),=C'** AFTER **'                                            
         MVC   P+14(80),0(R5)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         AP    DARE,=P'1'                                                       
*                                                                               
         B     DMXKEEP                                                          
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*******************************************************************             
*              END OF FILE                                        *             
*******************************************************************             
         SPACE 1                                                                
         USING RECD,R3                                                          
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(6),=C'DARE  '                                                
         EDIT  (P5,DARE),(7,P+11)                                               
         GOTO1 VPRINTER                                                         
         MVC   P+3(6),=C'MKGD'                                                  
         EDIT  (P5,MKGD),(7,P+11)                                               
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+3(33),=C'DETAILS OF PURGED RECORDS FOLLOWS'                    
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
DARE     DC    PL5'0'                                                           
MKGD     DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF REP CODES TO PURGE                                                   
*                                                                               
PRGLST   DS    0CL2                                                             
         DC    C'YH'                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
RECUP    DS    V                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENMKG                                                       
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092RELDXML   09/16/05'                                      
         END                                                                    
         PRINT NOGEN                                                            
LDCPTR   CSECT                                                                  
         NMOD1 WRKDLEN,RELDCPTR,R8,R9                                           
         USING WRKD,RC                                                          
         SPACE 1                                                                
         ST    R1,SAVER1                                                        
         BC    0,PPSTART                                                        
         OI    *-3,X'F0'           THIS CODE ONLY ONE TIME                      
         SPACE 1                                                                
         L     R6,=V(DATEVAL)                                                   
         GOTO1 =V(DATCON),DMCB,(4,0(R6)),(0,TODAY)                              
         L     R6,=F'-7'                                                        
         GOTO1 =V(ADDAY),DMCB,TODAY,WORK,(R6)                                   
         GOTO1 =V(DATCON),DMCB,(0,WORK),(3,SARDAT)                              
         EJECT                                                                  
PPSTART  L     R1,SAVER1                                                        
         LM    R2,R3,0(R1)         R2=A(RECORD)  R3=A(DIRECTORY PTR)            
         SPACE 1                                                                
         MVC   HALF,27(R2)                                                      
         LH    RE,HALF                                                          
         LA    RE,0(RE,R2)                                                      
         MVI   0(RE),0                                                          
         SPACE 1                                                                
         MVC   0(27,R3),0(R2)      SET ACTIVE PTR                               
         MVC   27(1,R3),29(R2)     STATUS FROM PARENT                           
         XC    28(4,R3),28(R3)                                                  
         LA    R3,32(R3)           BUMP TO NEXT PTR                             
         B     PPTEST                                                           
         SPACE 2                                                                
EXIT     MVI   0(R3),0             SET END OF PTRS                              
         XMOD1 1                                                                
         SPACE 2                                                                
PPTEST   DS    0H                                                               
*                                                                               
         CLI   0(R2),X'01'         REP RECORD?                                  
         BE    MSTRAP              YES - TRAP MASTER/SUBSIDIARY                 
         CLI   0(R2),X'02'                                                      
         BE    PPSTA                                                            
         CLI   0(R2),X'08'                                                      
         BE    PPADV                                                            
         CLI   0(R2),X'09'                                                      
         BE    PPPRD                                                            
         CLI   0(R2),X'0A'                                                      
         BE    PPAGY                                                            
         CLI   0(R2),X'1A'                                                      
         BE    PPAGYDAR                                                         
         CLI   0(R2),X'0B'                                                      
         BE    PPBUY                                                            
         CLI   0(R2),X'0C'                                                      
         BE    PPCON                                                            
         CLI   0(R2),X'11'         PASSIVES FOR MAKEGOOD RECS                   
         BE    PPMAKEGD                                                         
         CLI   0(R2),X'12'                                                      
         BE    PPINV                                                            
         CLI   0(R2),X'2B'                                                      
         BE    PPMKT                                                            
         CLI   0(R2),X'43'                                                      
         BE    PPPRO                                                            
         CLI   0(R2),X'06'                                                      
         BE    PPSAL                                                            
         CLI   0(R2),X'41'         PASSIVES FOR DARE AGENCY ORDERS              
         BE    PPDAR                                                            
         CLI   0(R2),X'51'         PASSIVES FOR DARE CONFIRMS                   
         BE    PPDAR                                                            
         B     EXIT                                                             
         EJECT                                                                  
MSTRAP   DS    0H                  REP RECORD                                   
         USING RREPREC,R2                                                       
         CLC   =C'NU',RREPKREP     CLEAR CHANNEL?                               
         BE    MSTR0100            YES - SPECIAL HANDLING                       
         CLC   =X'FFFF',RREPMAST   MASTER RECORD?                               
         BNE   EXIT                                                             
         LA    RF,RREPELEM         X'01' ELEMENT                                
         ZIC   RE,1(RF)                                                         
         AR    RF,RE               BUMP TO NEXT ELEMENT                         
         CLI   0(RF),2             MUST BE X'02' ELEMENT                        
         BNE   EXIT                                                             
*                                                                               
*   THIS SHOULD DUMP IF X'02' NOT FOUND, BUT THE RELOAD SHOULD                  
*        NOT ABEND.  AS THIS AFFECTS THE DARE INBOX PASSIVE                     
*        KEYS ONLY, THE JOB WILL CONTINUE.                                      
*                                                                               
         LA    RE,POWERCDS         SET A(POWER CODE STORE)                      
MSTR0020 EQU   *                                                                
         CLI   0(RE),C' '          EMPTY SLOT?                                  
         BE    MSTR0040            YES - INSERT HERE                            
         LA    RE,4(RE)            BUMP TO NEXT SLOT                            
*                                  ROOM FOR 32 MASTER/SUBS                      
*                                  THIS SHOULD NEVER BE EXHAUSTED               
         B     MSTR0020            GO BACK FOR NEXT                             
MSTR0040 EQU   *                                                                
         ZIC   R0,2(RF)            SET LOOP COUNT # SUBSIDIARIES                
         LA    RF,10(RF)           BUMP TO 1ST SUBSIDIARY                       
MSTR0060 EQU   *                                                                
         MVC   0(2,RE),0(RF)       INSERT SUB INTO TABLE                        
         MVC   2(2,RE),RREPKREP    INSERT MASTER REP                            
         LA    RE,4(RE)            BUMP TO NEXT SLOT                            
         LA    RF,2(RF)            BUMP TO NEXT SUBSIDIARY                      
         BCT   R0,MSTR0060         INSERT NEXT INTO TABLE                       
         B     EXIT                FINISHED                                     
MSTR0100 EQU   *                                                                
         LA    RE,POWERCDS         SET A(POWER CODE STORE)                      
MSTR0120 EQU   *                                                                
         CLI   0(RE),C' '          EMPTY SLOT?                                  
         BE    MSTR0140            YES - INSERT HERE                            
         LA    RE,4(RE)            BUMP TO NEXT SLOT                            
*                                  ROOM FOR 32 MASTER/SUBS                      
*                                  THIS SHOULD NEVER BE EXHAUSTED               
         B     MSTR0120            GO BACK FOR NEXT                             
MSTR0140 EQU   *                                                                
         MVC   0(4,RE),=C'NUK3'    SET UP CLEAR CHANNEL AS A                    
*                                     KATZ RADIO SUBSIDIARY                     
         B     EXIT                FINISHED                                     
         DROP  R2                                                               
PPSTA    DS    0H                  STATION                                      
         USING RSTAREC,R2                                                       
*                                                                               
         LA    RF,RSTAELEM                                                      
PPSTA010 EQU   *                                                                
         CLI   0(RF),0                                                          
         BE    PPSTA012                                                         
         CLI   0(RF),X'51'         SKIP PASSIVES IF ELEMENTS FOUND              
         BE    PPSTAX                                                           
         CLI   0(RF),X'52'                                                      
         BE    PPSTAX                                                           
         CLI   0(RF),X'53'                                                      
         BE    PPSTAX                                                           
*                                                                               
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     PPSTA010                                                         
*                                                                               
PPSTA012 EQU   *                                                                
         XC    0(32,R3),0(R3)                                                   
         MVI   RST2KTYP-RST2KEY(R3),X'82'                                       
         MVC   RST2KSTA-RST2KEY(5,R3),RSTAKSTA                                  
         MVC   RST2KEND-RST2KEY(3,R3),RSTAEND                                   
         MVC   RST2KREP-RST2KEY(2,R3),RSTAKREP                                  
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
*                                                                               
         CLI   RSTAKSTA+4,C'C'     COMBO PARENT STATION?                        
         BNE   PPSTA040            NO  - DON'T GENERATE NEXT KEY                
*                                  YES - FIND X'0A' 'CHILD' ELEMENT(S)          
         LA    RF,RSTAELEM                                                      
PPSTA020 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PPSTA040            YES - FINISHED W/CHILDREN KEYS               
         CLI   0(RF),X'0A'         'CHILD' ELEMENT?                             
         BNE   PPSTA030            NO                                           
*                                                                               
         CLI   7(RF),C'-'          NON-PARTICIPATING?                           
         BE    PPSTA030            YES                                          
*                                                                               
         XC    0(32,R3),0(R3)                                                   
                                                                                
         MVI   RST3KTYP-RST3KEY(R3),X'83'                                       
*                                  SET PASSIVE KEY TYPE                         
         MVI   RST3KSTP-RST3KEY(R3),X'01'                                       
*                                  SET PASSIVE KEY SUBTYPE                      
         MVC   RST3KREP-RST3KEY(2,R3),RSTAKREP                                  
*                                  INSERT REP CODE                              
         MVC   RST3KCST-RST3KEY(5,R3),2(RF)                                     
*                                  INSERT 'CHILD' STATION                       
         MVC   RST3KPST-RST3KEY(5,R3),RSTAKSTA                                  
*                                  INSERT 'PARENT' STATION                      
         LA    R3,32(R3)                                                        
*                                                                               
PPSTA030 EQU   *                                                                
         ZIC   RE,1(RF)            BUMP TO NEXT ELEMENT                         
         AR    RF,RE                                                            
         B     PPSTA020            GO BACK FOR NEXT ELEMENT                     
*                                                                               
PPSTA040 EQU   *                                                                
         XC    0(32,R3),0(R3)      MARKET NAME PASSIVE                          
                                                                                
         MVI   RST4KTYP-RST4KEY(R3),X'83'                                       
*                                  SET PASSIVE KEY TYPE                         
         MVI   RST4KSTP-RST4KEY(R3),X'02'                                       
*                                  SET PASSIVE KEY SUBTYPE                      
         MVC   RST4KREP-RST4KEY(2,R3),RSTAKREP                                  
*                                  INSERT REP CODE                              
         MVC   RST4KMKT-RST4KEY(18,R3),RSTAMKT                                  
*                                  INSERT 1ST 18 CHARS OF MKT NAME              
         MVC   RST4KSTA-RST4KEY(5,R3),RSTAKSTA                                  
*                                  INSERT STATION                               
         LA    R3,32(R3)                                                        
*                                                                               
         OC    RSTAOWN,RSTAOWN                                                  
         BZ    PPSTA050                                                         
*                                                                               
         XC    0(32,R3),0(R3)      OWNER PASSIVE                                
                                                                                
         MVI   RST5KTYP-RST5KEY(R3),X'83'                                       
*                                  SET PASSIVE KEY TYPE                         
         MVI   RST5KSTP-RST5KEY(R3),X'03'                                       
*                                  SET PASSIVE KEY SUBTYPE                      
         MVC   RST5KREP-RST5KEY(2,R3),RSTAKREP                                  
*                                  INSERT REP CODE                              
         MVC   RST5KOWN-RST5KEY(3,R3),RSTAOWN                                   
*                                  INSERT OWNER                                 
         MVC   RST5KSTA-RST5KEY(5,R3),RSTAKSTA                                  
*                                  INSERT STATION                               
         LA    R3,32(R3)                                                        
*                                                                               
PPSTA050 EQU   *                                                                
         OC    RSTAOWN,RSTAOWN                                                  
         BZ    PPSTA060                                                         
*                                                                               
         XC    0(32,R3),0(R3)      OWNER/MARKET PASSIVE                         
                                                                                
         MVI   RST6KTYP-RST6KEY(R3),X'83'                                       
*                                  SET PASSIVE KEY TYPE                         
         MVI   RST6KSTP-RST6KEY(R3),X'04'                                       
*                                  SET PASSIVE KEY SUBTYPE                      
         MVC   RST6KREP-RST6KEY(2,R3),RSTAKREP                                  
*                                  INSERT REP CODE                              
         MVC   RST6KOWN-RST6KEY(3,R3),RSTAOWN                                   
*                                  INSERT OWNER                                 
         MVC   RST6KSTA-RST6KEY(5,R3),RSTAKSTA                                  
*                                  INSERT STATION                               
         LA    RF,RSTAELEM                                                      
PPSTA052 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PPSTA060            YES - SKIP KEY                               
         CLI   0(RF),X'08'         EXTENDED DESCRIPTION ELEMENT?                
         BE    PPSTA054            YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PPSTA052                                                         
*                                                                               
PPSTA054 DS    0H                                                               
         MVC   RST6KMKT-RST6KEY(4,R3),RSTAMKTC-RSTAXXEL(RF)                     
*                                  INSERT MARKET CODE                           
         LA    R3,32(R3)                                                        
*                                                                               
***>>>  PASSIVE 8306                                                            
PPSTA060 EQU   *                                                                
*                                                                               
         XC    0(32,R3),0(R3)      MARKET/STATION PASSIVE                       
                                                                                
         MVI   RST8KTYP-RST8KEY(R3),X'83'                                       
*                                  SET PASSIVE KEY TYPE                         
         MVI   RST8KSTP-RST8KEY(R3),X'06'                                       
*                                  SET PASSIVE KEY SUBTYPE                      
         MVC   RST8KREP-RST8KEY(2,R3),RSTAKREP                                  
*                                  INSERT REP CODE                              
         MVC   RST8KSTA-RST8KEY(5,R3),RSTAKSTA                                  
*                                  INSERT STATION                               
         LA    RF,RSTAELEM                                                      
PPSTA062 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PPSTA070            YES - SKIP KEY                               
         CLI   0(RF),X'08'         EXTENDED DESCRIPTION ELEMENT?                
         BE    PPSTA064            YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PPSTA062                                                         
*                                                                               
PPSTA064 DS    0H                                                               
         CLC   RSTAMKTC-RSTAXXEL(4,RF),=C'    '                                 
*                                  ANY MARKET CODE ENTERED?                     
         BNH   PPSTA070            NO                                           
         MVC   RST8KMKT-RST8KEY(4,R3),RSTAMKTC-RSTAXXEL(RF)                     
*                                  INSERT MARKET CODE                           
         LA    R3,32(R3)                                                        
*                                                                               
PPSTA070 EQU   *                                                                
***>>>  END PASSIVE 8306                                                        
***>>>  PASSIVE 8307                                                            
*                                                                               
         XC    0(32,R3),0(R3)      SIGNON ID PASSIVE KEY                        
                                                                                
         LA    RF,RSTAELEM                                                      
PPSTA072 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PPSTA080            YES - SKIP KEY                               
         CLI   0(RF),X'06'         EXTENDED DESCRIPTION ELEMENT?                
         BE    PPSTA074            YES                                          
*                                                                               
PPSTA073 EQU   *                                                                
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PPSTA072                                                         
*                                                                               
PPSTA074 DS    0H                                                               
         MVI   RST9KTYP-RST9KEY(R3),X'83'                                       
*                                  SET PASSIVE KEY TYPE                         
         MVI   RST9KSTP-RST9KEY(R3),X'07'                                       
*                                  SET PASSIVE KEY SUBTYPE                      
         MVC   RST9KREP-RST9KEY(2,R3),RSTAKREP                                  
*                                  INSERT REP CODE                              
         MVC   RST9KSTA-RST9KEY(5,R3),RSTAKSTA                                  
*                                  INSERT STATION                               
         MVC   RST9KSIG-RST9KEY(2,R3),RSTASID-RSTASOEL(RF)                      
*                                  INSERT SIGNON ID CODE                        
         LA    R3,32(R3)                                                        
         B     PPSTA073            GO BACK FOR NEXT X'06' ELEMENT               
*                                                                               
PPSTA080 EQU   *                                                                
***>>>  END PASSIVE 8307                                                        
***>>>  PASSIVE 8308                                                            
*                                                                               
         XC    0(32,R3),0(R3)      SIGNON ID PASSIVE KEY                        
                                                                                
         LA    RF,RSTAELEM                                                      
PPSTA082 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PPSTA090            YES - SKIP KEY                               
         CLI   0(RF),X'2A'         UNIQUE ID ELEMENT?                           
         BE    PPSTA084            YES                                          
*                                                                               
PPSTA083 EQU   *                                                                
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     PPSTA082                                                         
*                                                                               
PPSTA084 DS    0H                                                               
         MVI   RSTUKTYP-RSTUKEY(R3),X'83'                                       
*                                  SET PASSIVE KEY TYPE                         
         MVI   RSTUKSTP-RSTUKEY(R3),X'08'                                       
*                                  SET PASSIVE KEY SUBTYPE                      
         MVC   RSTUKREP-RSTUKEY(2,R3),RSTAKREP                                  
*                                  INSERT REP CODE                              
         MVC   RSTUKSTA-RSTUKEY(5,R3),RSTAKSTA                                  
*                                  INSERT STATION                               
         MVC   RSTUKUID-RSTUKEY(6,R3),RSTAUIST-RSTAUIEL(RF)                     
*                                  INSERT UNIQUE ID CODE                        
         LA    R3,32(R3)           BUMP TO NEXT KEY SLOT                        
*                                                                               
PPSTA090 EQU   *                                                                
***>>>  END PASSIVE 8308                                                        
         XC    0(32,R3),0(R3)      OWNER/MARKET PASSIVE                         
*                                                                               
PPSTAX   EQU   *                                                                
         B     EXIT                                                             
         DROP  R2                                                               
         SPACE 4                                                                
PPADV    DS    0H                  ADVERTISER                                   
         USING RADVREC,R2                                                       
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'88'                                                      
         MVC   21(4,R3),RADVKADV                                                
         MVC   25(2,R3),RADVKREP                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R4,RADVELEM                                                      
         SR    R5,R5                                                            
PPADV1   CLI   0(R4),0             FIND ADVERTISER ELEMENT                      
         BNE   *+6                                                              
         DC    H'0'                NO ELEM                                      
         CLI   0(R4),X'01'                                                      
         BE    PPADV2                                                           
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PPADV1                                                           
PPADV2   MVC   1(20,R3),2(R4)      ADVERTISER NAME TO PP                        
PPADVX   LA    R3,32(R3)                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
PPSAL    DS    0H                  SALESMAN                                     
         USING RSALREC,R2                                                       
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'86'                                                      
         MVC   2(2,R3),RSALKREP                                                 
         MVC   4(20,R3),RSALNAME                                                
         MVC   24(3,R3),RSALKSAL                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
*                                                                               
         XC    0(32,R3),0(R3)      2ND PP                                       
         MVC   0(2,R3),=X'8601'                                                 
         MVC   2(2,R3),RSALKREP                                                 
         MVC   22(2,R3),RSALTEAM                                                
         MVC   24(3,R3),RSALKSAL                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*   INCLUSION OF CODE SUPPORTING GENERATION OF DARE PASSIVE KEYS.               
*        COMMON SOURCE IS INTENDED TO REDUCE LIKELIHOOD OF ERROR.               
*                                                                               
       ++INCLUDE REDARPAS                                                       
*                                                                               
         EJECT                                                                  
PPPRD    DS    0H                  PRODUCT                                      
         USING RPRDREC,R2                                                       
         XC    0(32,R3),0(R3)                                                   
*                                                                               
         OC    RPRDNET#,RPRDNET#                                                
         BZ    EXIT                                                             
         CLC   RPRDNET#,SPACES                                                  
         BE    EXIT                                                             
         MVI   RPR2KEY-RPR2KEY(R3),X'89'                                        
         MVC   RPR2KREP-RPR2KEY(2,R3),RPRDKREP                                  
         MVC   RPR2KNET-RPR2KEY(8,R3),RPRDNET#                                  
         MVC   RPR2KADV-RPR2KEY(4,R3),RPRDKADV                                  
         MVC   RPR2KPRD-RPR2KEY(3,R3),RPRDKPRD                                  
         MVC   27(1,R3),29(R2)     STATUS                                       
PPPRDX   LA    R3,32(R3)                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         SPACE 4                                                                
PPAGY    DS    0H                  AGENCY                                       
         USING RAGYREC,R2                                                       
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'8A'                                                      
         MVC   19(4,R3),RAGYKAGY                                                
         MVC   23(2,R3),RAGYKAOF                                                
         MVC   25(2,R3),RAGYKREP                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R4,RAGYELEM                                                      
         SR    R5,R5                                                            
PPAGY1   CLI   0(R4),0             FIND AGENCY ELEMENT                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'01'                                                      
         BE    PPAGY2                                                           
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PPAGY1                                                           
PPAGY2   MVC   1(18,R3),2(R4)      AGENCY NAME TO PP                            
PPAGYX   LA    R3,32(R3)                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
PPAGYDAR DS    0H                  AGENCY2 DARE POINTER                         
         USING RAGY2REC,R2                                                      
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'9A'                                                      
         MVC   14(2,R3),RAGK2REP                                                
         MVC   21(4,R3),RAGK2AGY                                                
         MVC   25(2,R3),RAGK2AOF                                                
         MVC   27(1,R3),RAGY2CTL   STATUS                                       
                                                                                
         OC    RAGY2DAR,RAGY2DAR   1ST DARE AGENCY                              
         BZ    PAGD0040                                                         
         CLC   RAGY2DAR,SPACES                                                  
         BE    PAGD0040                                                         
         MVC   16(5,R3),RAGY2DAR   AGENCY + OFF                                 
         OC    16(5,R3),SPACES     SPACE PAD                                    
         LA    R3,32(R3)                                                        
                                                                                
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'9A'                                                      
         MVC   14(2,R3),RAGK2REP                                                
         MVC   21(4,R3),RAGK2AGY                                                
         MVC   25(2,R3),RAGK2AOF                                                
         MVC   27(1,R3),RAGY2CTL   STATUS                                       
                                                                                
         OC    RAGY2DR2,RAGY2DR2   2ND DARE AGENCY                              
         BZ    PAGD0040                                                         
         CLC   RAGY2DR2,SPACES                                                  
         BE    PAGD0040                                                         
         MVC   16(5,R3),RAGY2DR2   AGENCY + OFF                                 
         OC    16(5,R3),SPACES     SPACE PAD                                    
         LA    R3,32(R3)                                                        
                                                                                
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'9A'                                                      
         MVC   14(2,R3),RAGK2REP                                                
         MVC   21(4,R3),RAGK2AGY                                                
         MVC   25(2,R3),RAGK2AOF                                                
         MVC   27(1,R3),RAGY2CTL   STATUS                                       
                                                                                
         OC    RAGY2DR3,RAGY2DR3   3RD DARE AGENCY                              
         BZ    PAGD0040                                                         
         CLC   RAGY2DR3,SPACES                                                  
         BE    PAGD0040                                                         
         MVC   16(5,R3),RAGY2DR3   AGENCY + OFF                                 
         OC    16(5,R3),SPACES     SPACE PAD                                    
         LA    R3,32(R3)                                                        
                                                                                
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'9A'                                                      
         MVC   14(2,R3),RAGK2REP                                                
         MVC   21(4,R3),RAGK2AGY                                                
         MVC   25(2,R3),RAGK2AOF                                                
         MVC   27(1,R3),RAGY2CTL   STATUS                                       
                                                                                
         OC    RAGY2DR4,RAGY2DR4   4TH DARE AGENCY                              
         BZ    PAGD0040                                                         
         CLC   RAGY2DR4,SPACES                                                  
         BE    PAGD0040                                                         
         MVC   16(5,R3),RAGY2DR4   AGENCY + OFF                                 
         OC    16(5,R3),SPACES     SPACE PAD                                    
         LA    R3,32(R3)                                                        
PAGD0040 EQU   *                                                                
         XC    0(32,R3),0(R3)                                                   
         OC    RAGY2TER,RAGY2TER   ANY TERRITORY IN RECORD?                     
         BZ    PAGD0200            NO  - EXIT                                   
         CLC   RAGY2TER,=C'  '     ANY TERRITORY IN RECORD?                     
         BE    PAGD0200            NO  - EXIT                                   
*                                                                               
         MVI   0(R3),X'AA'         SET UP AGY/TERR KEY                          
         MVC   17(2,R3),RAGK2REP                                                
         MVC   19(2,R3),RAGY2TER   TERRITORY                                    
         MVC   21(4,R3),RAGK2AGY                                                
         MVC   25(2,R3),RAGK2AOF                                                
         LA    R3,32(R3)                                                        
PAGD0200 DS    0H                                                               
         XC    0(32,R3),0(R3)                                                   
         LA    R4,RAGY2REC+34      FIRST ELEM                                   
         SR    R5,R5                                                            
PAGD0220 CLI   0(R4),0             FIND AGENCY ELEMENT                          
         BE    PAGD0300            NO ELEM - CAN'T GEN PTR                      
         CLI   0(R4),X'1F'                                                      
         BE    PAGD0230                                                         
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PAGD0220                                                         
         USING RAG2ELEM,R4                                                      
PAGD0230 DS    0H                                                               
         MVI   0(R3),X'BA'         REC TYPE                                     
         MVC   1(18,R3),RAG2NAM1   AGY NAME (FIRST 18 CHAR)                     
         DROP  R4                                                               
         MVC   19(8,R3),RAGK2AGY   AGY CODE, OFFICE, REP CODE                   
         LA    R3,32(R3)                                                        
PAGD0300 DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
PPBUY    DS    0H                  BUY                                          
*                                                                               
*   BUY KEY = X'0B00'                                                           
*   SHADOW RECORDS ARE X'0B0X' WHERE 'X' IS ANYTHING OTHER THAN                 
*        BINARY ZERO                                                            
*                                                                               
         CLI   1(R2),0             ELIMINATE SHADOW BUY RECORDS                 
         BH    EXIT                                                             
         USING RBUYREC,R2                                                       
         LA    R4,RBUYREC          A(BUY RECORD)                                
         ZICM  RF,RBUYLEN,2        L(BUY RECORD)                                
         LR    R5,R4                                                            
         AR    R5,RF               R5 = A(END OF RECORD)                        
         LA    R4,RBUYELEM         POINT TO THE FIRST ELEMENT                   
PPBUY10  EQU   *                                                                
         CR    R4,R5               END OF RECORD?                               
         BE    EXIT                YES - FINISHED                               
         CLI   0(R4),0             ELEMENT CODE = X'00'?                        
         BE    EXIT                YES - FINISHED                               
         CLI   0(R4),X'08'         SPOTPAK ELEMENT?                             
         BE    PPBUY20             YES, BUILD A KEY                             
         CLI   0(R4),X'5F'         BUYLINE CODE ELEMENT?                        
         BE    PPBUY30             YES, BUILD A KEY                             
PPBUY15  EQU   *                                                                
         ZIC   RF,1(R4)            FIND NEXT ELEMENT                            
         AR    R4,RF                                                            
         B     PPBUY10                                                          
         SPACE 1                                                                
PPBUY20  EQU   *                                                                
         XC    0(32,R3),0(R3)      1ST PP                                       
*                                                                               
         USING RBUYSPEL,R4                                                      
         MVI   0(R3),X'9B'                                                      
         MVC   09(2,R3),RBUYKREP                                                
         MVC   11(4,R3),RBUYSADV                                                
         MVC   15(3,R3),RBUYSPRD                                                
         MVC   18(4,R3),RBUYKCON                                                
         MVC   22(3,R3),RBUYKPLN                                                
         MVC   25(1,R3),RBUYKMLN                                                
         MVC   26(1,R3),RBUYKLIN                                                
         LA    R3,32(R3)                                                        
         B     PPBUY15             GO BACK FOR NEXT ELEMENT                     
         DROP  R4                                                               
PPBUY30  EQU   *                                                                
         USING RBYSCDEL,R4                                                      
         XC    0(32,R3),0(R3)      2ND PP                                       
*                                                                               
         MVI   0(R3),X'9B'         TWO-BYTE RECORD TYPE                         
         MVI   1(R3),X'01'                                                      
         MVC   11(2,R3),RBUYKREP   INSERT REP CODE FROM REC KEY                 
         MVC   13(3,R3),RBYSCDBC   INSERT BUYLINE CODE FROM ELEMENT             
*                                                                               
*   REVERSE AND 9'S COMP BUY RECORD'S CONTRACT NUMBER                           
*                                                                               
         ZAP  WORK(5),=P'99999999'                                              
                                                                                
         PACK  WORK+10(1),RBUYKCON+3(1)      INVERT THE SEQUENCE                
         PACK  WORK+11(1),RBUYKCON+2(1)      OF THE DIGITS                      
         PACK  WORK+12(1),RBUYKCON+1(1)                                         
         PACK  WORK+13(1),RBUYKCON(1)        MOVE IN SIGN AND                   
*                                                                               
         MVI   WORK+14,X'0C'                 SHIFT ONE DECIMAL PLACE            
         SRP   WORK+10(5),64-1,0             TO RIGHT                           
         SP    WORK(5),WORK+10(5)            BEFORE SUBTRACTING FROM            
         SRP   WORK(5),1,0                   NINES AND SHIFTING 1 TO            
*                                               THE LEFT                        
         MVC   16(4,R3),WORK       INSERT REV'D,COMP'D CON #                    
         MVC   20(1,R3),RBUYKLIN   INSERT BUYLINE #                             
         MVC   21(3,R3),=X'FFFFFF' INITIALIZE EARLIEST DATE                     
*                                  LATEST DATE ALREADY SET TO BIN ZERO          
         LA    RF,RBUYELEM                                                      
PPBUY100 EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    PPBUY200            YES - NO (MORE) X'03' ELEMENT(S)             
         CLI   0(RF),X'03'         EFFECTIVE DATE ELEMENT?                      
         BE    PPBUY140            YES - ADJUST PASSIVE KEYS                    
PPBUY120 EQU   *                                                                
         ZIC   RE,1(RF)            NO  - BUMP TO NEXT ELEMENT                   
         AR    RF,RE                                                            
         B     PPBUY100                                                         
PPBUY140 EQU   *                                                                
         CLC   RBUYDTST-RBUYDTEL(3,RF),21(R3)                                   
*                                  EFF START DATE EARLIER?                      
         BH    PPBUY160            NO                                           
         MVC   21(3,R3),RBUYDTST-RBUYDTEL(RF)                                   
*                                  YES - USE IT                                 
PPBUY160 EQU   *                                                                
         CLC   RBUYDTED-RBUYDTEL(3,RF),24(R3)                                   
*                                  EFF END   DATE LATER?                        
         BNH   PPBUY180            NO                                           
         MVC   24(3,R3),RBUYDTED-RBUYDTEL(RF)                                   
*                                  YES - USE IT                                 
PPBUY180 EQU   *                                                                
         B     PPBUY120            GO BACK FOR NEXT X'03' ELEMENT               
PPBUY200 DS    0H                                                               
*                                                                               
         LA    R3,32(R3)                                                        
         B     PPBUY15             GO BACK FOR NEXT ELEMENT                     
         DROP  R4                                                               
         DROP  R2                                                               
         EJECT                                                                  
PPCON    DS    0H                  CONTRACT                                     
         USING RCONREC,R2                                                       
*                                                                               
         XC    0(32,R3),0(R3)      1ST PP                                       
         MVI   0(R3),X'8C'                                                      
         MVC   21(2,R3),RCONKREP                                                
         ZAP   DUB,=P'99999999'    CALCULATE 95 COMPLEMENT                      
         ZAP   DUB1,=P'0'                                                       
         MVO   DUB1,RCONKCON                                                    
         SP    DUB,DUB1                                                         
         LM    R4,R5,DUB                                                        
         SRDL  R4,4                                                             
         ST    R5,DUB                                                           
         MVC   23(4,R3),DUB                                                     
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
                                                                                
*                                                                               
         XC    0(32,R3),0(R3)      2ND PP                                       
         MVI   0(R3),X'9C'                                                      
         MVC   02(2,R3),RCONKREP                                                
         MVC   04(2,R3),RCONKOFF                                                
         MVC   06(2,R3),RCONKGRP                                                
         MVC   08(5,R3),RCONKSTA                                                
         MVC   13(4,R3),RCONKADV                                                
         MVC   17(4,R3),RCONKAGY                                                
         MVC   21(2,R3),RCONKAOF                                                
         MVC   23(4,R3),RCONKCON                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
*                                                                               
         LA    R4,RCONELEM                                                      
PPC9C10  CLI   0(R4),0                                                          
         BE    PPC2C10                                                          
         CLI   0(R4),X'2A'         MOVE HISTORY ELEMENT                         
         BE    PPC9C20                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPC2C10                                                          
         AR    R4,R1                                                            
         B     PPC9C10             GO TO LOOP                                   
*                                                                               
PPC9C20  DS    0H                                                               
         USING RCONMMEL,R4                                                      
         XC    0(32,R3),0(R3)      MOVE PASSIVE KEY                             
         MVI   0(R3),X'9E'                                                      
         MVC   12(2,R3),RCONKREP                                                
         MVC   14(5,R3),RCONKSTA                                                
         MVC   19(2,R3),RCONMMOR                                                
         MVC   21(2,R3),RCONMMDT                                                
         MVC   23(4,R3),RCONMMOC                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
         XC    0(32,R3),0(R3)      MOVE PASSIVE KEY                             
         MVI   0(R3),X'A2'         SET UP X'A201' KEY                           
         MVI   1(R3),X'01'                                                      
         MVC   12(2,R3),RCONKREP                                                
         MVC   14(5,R3),RCONKSTA                                                
         MVC   19(2,R3),RCONMMOR                                                
****>>>  MVC   21(2,R3),RCONMMDT   NO DATE IN THIS KEY                          
         MVC   23(4,R3),RCONMMOC                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
         DROP  R4                                                               
*                                                                               
PPC2C10  DS    0H                                                               
         LA    R4,RCONELEM                                                      
PPC2C15  CLI   0(R4),0                                                          
         BE    PPCAC10                                                          
         CLI   0(R4),X'2C'         MOVE HISTORY/BB ELEMENT                      
         BE    PPC2C20                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPCAC10                                                          
         AR    R4,R1                                                            
         B     PPC2C15             GO TO LOOP                                   
*                                                                               
PPC2C20  DS    0H                                                               
         USING RCON2CEL,R4                                                      
         XC    0(32,R3),0(R3)      MOVE PASSIVE KEY                             
         MVI   0(R3),X'A2'         SET UP X'A202' KEY                           
         MVI   1(R3),X'02'                                                      
         MVC   12(2,R3),RCONKREP                                                
         MVC   14(5,R3),RCONKSTA                                                
         MVC   19(2,R3),RCON2COR                                                
****>>>  MVC   21(2,R3),RCON2CDT   NO DATE IN THIS KEY                          
         MVC   23(4,R3),RCON2COC                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
         DROP  R4                                                               
*                                                                               
***2CGEN                                                                        
PPCAC10  DS    0H                                                               
         XC    0(32,R3),0(R3)      3RD PP                                       
         MVI   0(R3),X'AC'                                                      
         MVC   01(2,R3),RCONKREP                                                
         MVC   03(2,R3),RCONKOFF                                                
         MVC   10(5,R3),RCONKSTA                                                
         MVC   15(4,R3),RCONKAGY                                                
         MVC   19(4,R3),RCONKADV                                                
         MVC   23(4,R3),RCONKCON                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         MVC   5(2,R3),RCONTEM                                                  
* INVERT SALESMAN CODE FOR LAST NAME HIGH                                       
         LA    RE,RCONSAL+2        LAST INITIAL                                 
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,R0                                                            
         MVC   07(1,R3),0(RE)                                                   
         MVC   08(2,R3),RCONSAL                                                 
         CLI   RCONSAL+2,C' '      ONLY 2 INITIALS?                             
         BNE   *+8                                                              
         MVI   9(R3),C' '                                                       
         MVC   DUB1(5),5(R3)                                                    
         LA    R3,32(R3)                                                        
*                                                                               
*   CODESWIT PASSIVE KEY GENERATION:  ALL CONTRACTS                             
*                                                                               
         XC    0(32,R3),0(R3)      BF PASSIVE                                   
         MVI   0(R3),X'BF'         INSERT CODE TYPE                             
         MVC   2(2,R3),RCONKREP    INSERT REP CODE                              
         MVC   4(4,R3),RCONKAGY    INSERT AGENCY CODE                           
         MVC   8(2,R3),RCONKAOF    INSERT AGENCY OFFICE CODE                    
         MVC   10(4,R3),RCONKADV   INSERT ADVERTISER                            
         MVC   14(5,R3),RCONKSTA   INSERT STATION                               
         MVC   19(2,R3),RCONKOFF   INSERT OFFICE                                
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE+3),(2,21(R3))                        
*                                  INSERT FLIGHT END DATE                       
         MVC   23(4,R3),RCONKCON   CURRENT CONTRACT NUMBER                      
*                                  INSERT CONTRACT NUMBER                       
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
*                                                                               
*                                                                               
* DARE TAKEOVER                                                                 
*                                                                               
         LA    R4,RCONELEM                                                      
PPCAD10  CLI   0(R4),0                                                          
         BE    PPCBC10                                                          
         CLI   0(R4),X'1C'         DARE TAKEOVER ELEMENT                        
         BE    PPCAD20                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPCBC10                                                          
         AR    R4,R1                                                            
         B     PPCAD10             GO TO LOOP                                   
*                                                                               
PPCAD20  DS    0H                                                               
         OC    4(4,R4),4(R4)                                                    
         BZ    PPCBC10                                                          
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'AD'                                                      
         MVC   12(2,R3),RCONKREP                                                
         MVC   14(5,R3),RCONKSTA                                                
         MVC   19(4,R3),4(R4)      PREVIOUS CONTRACT NUMBER                     
         MVC   23(4,R3),RCONKCON   CURRENT CONTRACT NUMBER                      
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
*                                                                               
PPCBC10  DS    0H                                                               
         XC    0(32,R3),0(R3)      4TH PP                                       
         MVI   0(R3),X'BC'                                                      
         MVC   2(2,R3),RCONKREP                                                 
         MVC   4(2,R3),RCONCTGY    CATEGORY                                     
         OC    RCONCTGY,RCONCTGY   TRANSITION                                   
         BNZ   *+10                                                             
         MVC   4(2,R3),=C'XX'                                                   
         MVC   6(2,R3),RCONKOFF                                                 
         MVC   8(5,R3),RCONKSTA                                                 
         MVC   13(4,R3),RCONKAGY                                                
         MVC   17(2,R3),RCONKAOF                                                
         MVC   19(4,R3),RCONKADV                                                
         MVC   23(4,R3),RCONKCON                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
*                                                                               
* GENERATE PASSIVE KEY FOR DARE CONTRACTS                                       
*                                                                               
         LA    R4,RCONELEM                                                      
PPCBD10  CLI   0(R4),0                                                          
         BE    PPCCC10                                                          
         CLI   0(R4),X'1D'         DARE ELEMENT                                 
         BE    PPCBD20                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPCCC10                                                          
         AR    R4,R1                                                            
         B     PPCBD10             GO TO LOOP                                   
*                                                                               
PPCBD20  DS    0H                                                               
         TM    2(R4),X'80'         MUST BE LINKED                               
         BZ    PPCCC10                                                          
         TM    2(R4),X'04'+X'02'   SKIP FOR EDI                                 
         BNZ   PPCCC10                                                          
         XC    0(32,R3),0(R3)                                                   
BDKEY    USING COND,R3                                                          
         MVI   BDKEY.RCONDATP,X'BD'                                             
         MVC   BDKEY.RCONDARP,RCONKREP                                          
         MVC   BDKEY.RCONDAAG,RCONKAGY                                          
         MVC   BDKEY.RCONDAAO,RCONKAOF                                          
         MVC   BDKEY.RCONDAAD,RCONKADV                                          
         MVC   BDKEY.RCONDAST,RCONKSTA                                          
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE),(2,BDKEY.RCONDAFS)                  
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE+3),(2,BDKEY.RCONDAFE)                
         MVC   BDKEY.RCONDACN,RCONKCON   CURRENT CONTRACT NUMBER                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
         DROP  BDKEY                                                            
*                                                                               
PPCCC10  DS    0H                                                               
         XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'CC'                                                      
         MVC   1(2,R3),RCONKREP                                                 
         MVC   3(5,R3),RCONKSTA                                                 
         MVC   8(2,R3),RCONKOFF                                                 
         MVC   10(5,R3),DUB1       FROM AC POINTER                              
         MVC   15(4,R3),RCONKADV                                                
         MVC   19(4,R3),RCONKAGY                                                
         MVC   23(4,R3),RCONKCON                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
*                                                                               
         EJECT                                                                  
                                                                                
*   CREATE X'8D' POINTERS                                                       
                                                                                
* - GET FLIGHT START/END DATES AND SAVE IN WORK+40                              
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE),(2,WORK+40)    START DATE           
         GOTO1 =V(DATCON),DMCB,(3,RCONDATE+3),(2,WORK+42)  END DATE             
                                                                                
* - GET DEMO FROM BOP OR SAR ELEMENT AND SAVE IN WORK+50                        
         XC    WORK+50(3),WORK+50                                               
         LA    R4,RCONELEM                                                      
PPC8DLP  CLI   0(R4),0                                                          
         BE    PPC8D00                                                          
         CLI   0(R4),X'12'         SAR ELEMENT                                  
         BE    PPC8DDD                                                          
         CLI   0(R4),X'10'         BOP ELEMENT                                  
         BE    PPC8DEE                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPC8D00                                                          
         AR    R4,R1                                                            
         B     PPC8DLP             GO TO LOOP                                   
         USING RSARCO,R4                                                        
PPC8DDD  LA    RE,RSARDEM                                                       
         LA    RF,8                                                             
         MVC   WORK+50(3),RSARDEM         DEMO                                  
PPC8DDE  TM    0(RE),X'40'                IS IT MARKED AS PRIMARY?              
         BO    PPC8DDF                    YES                                   
         LA    RE,3(RE)                                                         
         MVC   WORK+50(3),0(RE)                                                 
         BCT   RF,PPC8DDE                                                       
         MVC   WORK+50(3),RSARDEM         NO/USE 1ST AS DEFAULT                 
PPC8DDF  NI    WORK+50,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR          
         B     PPC8D00                                                          
                                                                                
         USING RCONBPEL,R4                                                      
PPC8DEE  LA    RE,RCONBPDM+1                                                    
         LA    RF,6                                                             
         MVC   WORK+50(3),RCONBPDM+1                                            
PPC8DEF  TM    WORK+50,X'40'           IS IT MARKED AS PRIMARY ?                
         BO    PPC8DEG                 YES                                      
         LA    RE,3(RE)                                                         
         MVC   WORK+50(3),0(RE)                                                 
         BCT   RF,PPC8DEF                                                       
         MVC   WORK+50(3),RCONBPDM+1    NO-USE 1ST AS DEFAULT                   
PPC8DEG  NI    WORK+50,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR          
         B     PPC8D00                                                          
         DROP R4                                                                
                                                                                
* BUILD BASIC KEY IN WORK                                                       
PPC8D00  XC    WORK(32),WORK                                                    
         LA    R4,WORK                                                          
         MVI   0(R4),X'8D'                                                      
         MVC   1(2,R4),RCONKREP                                                 
         MVC   8(2,R4),WORK+40    START DATE                                    
         MVC   10(2,R4),WORK+42    END DATE                                     
         MVC   12(4,R4),RCONKCON   CONTRACT NUMBER                              
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R3),WORK                                                    
         MVI   16(R3),1                                                         
         MVC   17(6,R3),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R3),RCONKADV       ADVERTISER                               
         LA    R3,32(R3)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R3),WORK                                                    
         MVI   16(R3),2                                                         
         MVC   17(3,R3),RCONSAL                                                 
         MVC   20(1,R3),RCONTYPE                                                
         MVC   21(2,R3),RCONKGRP                                                
         MVC   23(2,R3),RCONCTGY                                                
         MVC   25(2,R3),RCONTEM                                                 
         LA    R3,32(R3)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R3),WORK                                                    
         MVI   16(R3),3                                                         
         MVC   17(2,R3),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 =V(DATCON),DMCB,(3,RCONHDRD),(2,22(R3))                          
         MVC   19(3,R3),WORK+50       DEMO                                      
PPC8DX   LA    R3,32(R3)                                                        
                                                                                
* END X'8D' PASSIVE POINTERS                                                    
                                                                                
                                                                                
                                                                                
*   CREATE X'8E' POINTERS                                                       
*   SIMILAR TO X'8D' POINTERS BUT HAVE STATION IN KEY INSTEAD OF 0'S            
                                                                                
                                                                                
* WORK HAS BASIC KEY- REPLACE ZEROS OF X'8D' WITH STATION                       
PPCON8E  MVI   WORK,X'8E'                                                       
         MVC   WORK+3(5),RCONKSTA                                               
                                                                                
* ID = 1 = AGENCY / ADVERTISER                                                  
         MVC   0(32,R3),WORK                                                    
         MVI   16(R3),1                                                         
         MVC   17(6,R3),RCONKAGY       AGENCY(4) AND OFFICE(2)                  
         MVC   23(4,R3),RCONKADV       ADVERTISER                               
         LA    R3,32(R3)                                                        
                                                                                
* ID = 2 = SALESPERSON/CONTRACT TYPE/GROUP-SUBG/CATEGORY/TEAM                   
         MVC   0(32,R3),WORK                                                    
         MVI   16(R3),2                                                         
         MVC   17(3,R3),RCONSAL                                                 
         MVC   20(1,R3),RCONTYPE                                                
         MVC   21(2,R3),RCONKGRP                                                
         MVC   23(2,R3),RCONCTGY                                                
         MVC   25(2,R3),RCONTEM                                                 
         LA    R3,32(R3)                                                        
                                                                                
* ID = 3 = OFFICE(SALESPERSON)/DEMO/CREATION DATE                               
         MVC   0(32,R3),WORK                                                    
         MVI   16(R3),3                                                         
         MVC   17(2,R3),RCONKOFF      SALESPERSON OFFICE                        
* CREATION DATE YMD -> BINARY                                                   
         GOTO1 =V(DATCON),DMCB,(3,RCONHDRD),(2,22(R3))                          
         MVC   19(3,R3),WORK+50    DEMO                                         
PPC8EX   LA    R3,32(R3)                                                        
                                                                                
* END X'8E' PASSIVE POINTERS                                                    
         EJECT                                                                  
* CREATE X'8F/9F' PASSIVE POINTER IF SONNET ELEMENT ON CONTRACT REC             
*                                                                               
* SONNET ELEM ?                                                                 
         LA    R4,RCONELEM                                                      
PP8F01   CLI   0(R4),X'1F'       X'1F'- ELEMENT - CHECK IF CONFIRMED            
         BE    PP8F02                                                           
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PP8F04            NO 1F ELEM - CAN'T BE CONFIRMED                
         AR    R4,R1                                                            
         B     PP8F01                                                           
PP8F02   DS    0H                                                               
         USING RCONXEL,R4                                                       
         OC    RCONTRF,RCONTRF     ANY TRAFFIC NUMBER?                          
         BZ    PP8F03              NO  - NOTHING TO PUT OUT                     
         XC    0(32,R3),0(R3)                                                   
         MVC   0(2,R3),=X'A203'    INSERT RECORD TYPE                           
         MVC   RCONTNRP-RCONTNTP(2,R3),RCONKREP                                 
*                                  INSERT REP CODE                              
         MVC   RCONTNT#-RCONTNTP(10,R3),RCONTRF                                 
*                                  INSERT TRAFFIC NUMBER                        
         MVC   RCONTNCN-RCONTNTP(4,R3),RCONKCON                                 
*                                  INSERT CONTRACT NUMBER                       
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
*                                                                               
PP8F03   DS    0H                                                               
         TM    RCONCONF,X'40'      CONFIRMED?                                   
         BO    PP8FEND             YES - DON'T CREATE POINTER                   
         DROP  R4                                                               
*                                                                               
* SONNET ELEM ?                                                                 
         LA    R4,RCONELEM                                                      
PP8F04   CLI   0(R4),X'A3'       X'A3'- SONNET ELEMENT                          
         BE    PP8F05                                                           
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PP8FEND           NO SONNET ELEM - DON'T CREATE POINTER          
         AR    R4,R1                                                            
         B     PP8F04                                                           
*                                                                               
PP8F05   DS    0H                                                               
         USING RCONSON,R4                                                       
         CLI   RCONSNST,X'FF'      COMPLETED SONNET CYCLE?                      
         BE    PP8FEND             YES - DON'T CREATE POINTER                   
                                                                                
*JRD     CLC   RCONSDTE,=X'C0D1'   ..CREATED BEFORE JUN17/96 ?                  
*JRD     BL    PP8FEND             ..THEN DON'T CREATE KEY                      
                                                                                
         XC    0(32,R3),0(R3)                                                   
K        USING RCON8FTP,R3                                                      
         MVI   K.RCON8FTP,X'8F'                                                 
         MVC   K.RCON8FRP,RCONKREP    REP                                       
         MVC   K.RCON8FSA,RCONKSTA    STATION                                   
         MVC   K.RCON8FIN,=C'NEW'    DEFAULT 'SENT TO' ID (BOXID)               
         MVC   K.RCON8FCN,RCONKCON   CONTRACT NUMBER                            
         MVC   K.RCON8FDT,RCONSDTE    CREATE DATE                               
         DROP  K                                                                
*                                                                               
         LA    RF,RCONELEM                                                      
         SR    RE,RE                                                            
PP8F06   CLI   0(RF),0             END OF REC?                                  
         BE    PP8F08                                                           
         CLI   0(RF),X'1E'         RANDOM FLAGS ELEM?                           
         BNE   PP8F07                                                           
         TM    RCONRF1-RCONRFEL(RF),X'20' LOCAL ORDER?                          
         BZ    PP8F08                                                           
         MVI   0(R3),X'9F'         MAKE THIS A 9F LOCAL ORDER KEY               
         B     PP8F08                                                           
PP8F07   IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     PP8F06                                                           
*                                                                               
PP8F08   DS    0H                                                               
         ZIC   R1,RCONSONL         LENGTH                                       
         CHI   R1,RCONSOVQ         NEW CONTRACT?                                
         BE    PP8FOK              YES - WE'RE DONE                             
*                                                                               
         AHI   R1,RCONSOVQ         ID/LEN COMMENT LENGTH                        
         CHI   R1,6                MUST BE AT LEAST 0NE MINI ELEM               
         BL    PP8FERR                                                          
                                                                                
         SR    R0,R0                                                            
         D     R0,=A(L'RCONSONM)   DIV BY LENGTH OF MINI ELEM                   
         LA    RE,RCONSONM         MEMBER MINI ELEM                             
PP8F10   OC    0(3,RE),0(RE)       MEMBER ID                                    
         BZ    PP8FERR             NO ID-ERROR                                  
         OC    3(2,RE),3(RE)       DATE ADDED ?                                 
         BZ    PP8F15              NO = 'SENT TO' THIS MEMBER                   
         LA    RE,6(RE)            YES/BUMP TO NEXT MINI ELEM                   
         BCT   R1,PP8F10                                                        
         B     PP8FERR                                                          
PP8F15   MVC   10(3,R3),0(RE)       SET ID TO KEY                               
         B     PP8FOK                                                           
                                                                                
PP8FERR  MVC   10(3,R3),=C'XXX'     ERROR ID - SHOULD NEVER GET HERE            
         B     PP8FOK               CREATE KEY RATHER THAN DYING                
                                                                                
PP8FOK   LA    R3,32(R3)           8F POINTER CREATED, BUMP                     
                                                                                
PP8FEND  DS    0H                                                               
         DROP  R4                                                               
* END OF 8F PASSIVE POINTER                                                     
         EJECT                                                                  
* CREATE 9D PASSIVE POINTER FOR RIS/PRODUCT                                     
PP9D01   DS    0H                                                               
         MVI   0(R3),X'9D'                                                      
         MVC   1(2,R3),RCONKREP                                                 
         MVC   3(5,R3),RCONKSTA                                                 
         MVC   8(4,R3),RCONKADV                                                 
         MVC   23(4,R3),RCONKCON                                                
* NOW SET PRODUCT NAME OR CODE INTO KEY                                         
         MVI   12(R3),X'FF'              SET DEFAULT TO LAST                    
         MVC   13(3,R3),RCONPRD                                                 
         LA    R4,RCONELEM         NOW LOOK FOR PRODUCT NAME                    
PP9D05   CLI   0(R4),X'05'                                                      
         BE    PP9D10                                                           
         CLI   1(R4),0                                                          
         BE    PP9DX                                                            
         ZIC   R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PP9D05                                                           
         USING RCONEXEL,R4                                                      
PP9D10   MVC   12(9,R3),RCONEXPR   SET PRODUCT NAME                             
         DROP  R4                                                               
PP9DX    LA    R3,32(R3)           9D POINTER CREATED, BUMP                     
                                                                                
* CREATE AB01 PASSIVE POINTER FOR PRODUCT IF CODE USED                          
         CLC   RCONPRD,=C'   '     ANY PRODUCT CODE ENTERED?                    
         BE    PPRODNO                                                          
         XC    0(32,R3),0(R3)                                                   
         MVC   0(2,R3),=X'AB01'                                                 
         MVC   10(2,R3),RCONKREP                                                
         MVC   12(4,R3),RCONKAGY                                                
         MVC   16(4,R3),RCONKADV                                                
         MVC   20(3,R3),RCONPRD                                                 
         MVC   23(4,R3),RCONKCON                                                
         LA    R3,32(R3)           AB01 POINTER CREATED, BUMP                   
                                                                                
         EJECT                                                                  
PPRODNO  EQU   *                                                                
         SR    R5,R5                                                            
         LA    R4,RCONELEM                                                      
PPCON1   CLI   0(R4),0                                                          
         BE    PPCONEC                                                          
         CLI   0(R4),X'10'                                                      
         BE    PPCON2                                                           
         BH    PPCONEC                                                          
         CLI   1(R4),0                                                          
         BE    PPCONEC                                                          
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PPCON1                                                           
         SPACE 1                                                                
         USING RCONBPEL,R4                                                      
PPCON2   XC    0(32,R3),0(R3)                                                   
         MVI   0(R3),X'DC'         CREATE BOP POINTER                           
         MVC   5(2,R3),RCONKREP                                                 
         MVC   7(4,R3),RCONKADV                                                 
         MVC   11(3,R3),RCONBPDT                                                
         MVC   14(4,R3),RCONBPRF                                                
         MVC   18(5,R3),RCONKSTA                                                
         MVC   23(4,R3),RCONKCON                                                
         MVC   27(1,R3),29(R2)     STATUS                                       
*                                                                               
         LA    R3,32(R3)                                                        
         XC    0(32,R3),0(R3)                                                   
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 1                                                                
PPCONEC  XC    0(32,R3),0(R3)                                                   
         TM    RCONCNTL,X'01'      IGNORE CLOSED RECORDS                        
         BO    EXIT                                                             
         LA    R4,RCONELEM                                                      
         SR    R5,R5                                                            
         SR    R6,R6                                                            
PPCONEC1 CLI   0(R4),0             END OF RECORD?                               
         BE    PPCONEC4            YES - CHECK FOR AN X'08' ELT                 
         CLI   0(R4),6             SPL ELEMENT?                                 
         BNE   *+6                 NO  -                                        
         LR    R6,R4               YES - SAVE A(SPL ELEMENT)                    
         CLI   0(R4),8             TRUE ACTIVITY ELEMENT?                       
         BNE   *+6                 NO  -                                        
         LR    R5,R4               YES - SAVE A(TRUE ACTIVITY ELT)              
         CLI   0(R4),X'12'         SAR ELEMENT?                                 
         BE    PPCONEC2            YES                                          
         BH    PPCONEC4            PAST LAST USEFUL ELEMENT                     
         CLI   1(R4),0             L(ELEMENT) = ZERO?                           
         BE    PPCONEC4            YES - END OF RECORD                          
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     PPCONEC1                                                         
         SPACE 1                                                                
PPCONEC2 LTR   R6,R6               ANY SPL ELEMENT?                             
         BZ    PPCONEC3            NO  - NOT COMPLETED                          
*                                                                               
         USING RCONSPEL,R6         YES - CHECK IT                               
         OC    RCONSPDT,RCONSPDT                                                
         BZ    PPCONEC3            ZERO SPL ENTRY DATE - NOT COMPLETED          
         CLC   RCONSPDT,SARDAT                                                  
         BH    PPCONEC5            NOT 7 DAYS OLD                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTR   R5,R5               ANY TRUE ACTIVITY DATE ELEMENT?              
         BZ    EXIT                NO                                           
*                                                                               
         USING RCONACEL,R5         YES - CHECK IT                               
         OC    RCONACTA,RCONACTA   ANY TRUE ACTIVITY DATE?                      
         BZ    EXIT                                                             
         CLC   RCONACTA,SARDAT     TRUE ACTIVITY DATE RECENT?                   
         BH    PPCONEC5            NOT 7 DAYS OLD                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         USING RSAREL,R4                                                        
PPCONEC3 OC    RSARRDT,RSARRDT     RESOLUTION DATE                              
         BZ    PPCONEC5            UNRESOLVED                                   
         CLC   RSARRDT,=X'540904'                                               
         BL    EXIT                RESOLVED PRIOR TO SIGNIFICANT DATE           
         B     PPCONEC5                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
PPCONEC4 EQU   *                                                                
         LTR   R5,R5               TRUE ACTIVITY DATE ELEMENT FOUND?            
         BZ    EXIT                NO  - FINISHED                               
*                                                                               
         USING RCONACEL,R5                                                      
         CLC   RCONACTA,SARDAT     TRUE ACT DATE VS SARDAT                      
         BNH   EXIT                TRUE ACT DATE > 7 DAYS OLD                   
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
         USING RCONATYP,R3                                                      
PPCONEC5 MVI   RCONATYP,X'EC'                                                   
         MVC   RCONAREP,RCONKREP-RCONREC(R2)                                    
         PACK  RCONACON(1),DUB+3(1)                                             
         PACK  RCONACON+1(1),DUB+2(1)    REVERSE COMPLEMENT                     
         PACK  RCONACON+2(1),DUB+1(1)                                           
         PACK  RCONACON+3(1),DUB(1)                                             
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 1                                                                
SARDAT   DC    XL3'00'                                                          
BOPDAT   DC    XL3'00'                                                          
         EJECT                                                                  
PPMAKEGD DS    0H                  MARKET RECORD                                
         USING RMKGREC,R2                                                       
         XC    SAVMKG,SAVMKG                                                    
         LA    R5,SAVMKG           BUILD MOST OF PP IN SAVMKG                   
         MVI   0(R5),X'A0'                                                      
         MVI   1(R5),X'11'                                                      
         MVC   9(2,R5),RMKGKREP                                                 
         MVC   16(5,R5),RMKGKSTA                                                
         MVC   21(4,R5),RMKGKCON                                                
         MVC   25(2,R5),RMKGKGRP                                                
         MVC   27(1,R5),29(R2)     STATUS                                       
         SPACE 1                                                                
         LA    R4,RMKGELEM         THEN FILL IN THE REST FROM ELEMENT           
PPMKG10  CLI   0(R4),0             END OF RECORD?                               
         BE    PPMKGX              YES - EXIT                                   
         CLI   0(R4),X'0A'         M/G SWITCH/DARE ELEMENT?                     
         BE    PPMKG20             YES                                          
         BH    PPMKGX              NO  - NOT IN RECORD                          
PPMKG15  ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     PPMKG10                                                          
         SPACE 1                                                                
         USING RMKGXEL,R4                                                       
PPMKG20  MVC   11(3,R5),RMKGXSAL   INSERT S/P INTO KEY                          
         MVC   DUB(3),RMKGXSAL     SAVE FOR NEXT PASSIVE                        
         MVC   14(2,R5),RMKGXTEM   INSERT TEAM INTO KEY                         
         MVC   DUB+3(2),RMKGXTEM   SAVE FOR NEXT PASSIVE                        
         MVC   0(32,R3),SAVMKG     MOVE NEW X'A011' KEY TO TABLE                
         LA    R3,32(R3)           BUMP TO NEXT POSITION                        
         MVI   0(R5),X'A1'         SET NEXT MG PASSIVE                          
         MVC   11(2,R5),DUB+3      INSERT TEAM INTO KEY (HIGH)                  
         MVC   13(3,R5),DUB        INSERT S/P  INTO KEY (LOW)                   
         MVC   0(32,R3),SAVMKG     MOVE NEW X'A111' KEY TO TABLE                
         LA    R3,32(R3)           BUMP TO NEXT POSITION                        
*                                                                               
         SPACE 2                                                                
PPMKGX   B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
SAVMKG   DC    XL32'00'                                                         
         EJECT                                                                  
PPMKT    DS    0H                  MARKET RECORD                                
         USING RMKTREC,R2                                                       
*                                                                               
*   BUILD AF PASSIVE FOR MARKET NAME                                            
*                                                                               
         XC    0(32,R3),0(R3)                                                   
         MVI   RMKT2TYP-RMKTREC(R3),X'AF'                                       
*                                  INSERT KEY CODE/LEN                          
         MVC   RMKT2REP-RMKTREC(2,R3),RMKTKREP                                  
*                                  INSERT REP CODE                              
         MVC   RMKT2NAM-RMKTREC(19,R3),RMKTNAME                                 
*                                  INSERT MARKET NAME                           
         MVC   RMKT2MKT-RMKTREC(4,R3),RMKTKMKT                                  
*                                  INSERT MARKET CODE                           
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)           BUMP TO NEXT SLOT                            
*                                                                               
         XC    SAVMKT,SAVMKT                                                    
         LA    R5,SAVMKT           BUILD MOST OF PP IN SAVMKT                   
         MVI   0(R5),X'8B'                                                      
         MVC   16(2,R5),RMKTKREP                                                
         MVC   23(4,R5),RMKTKMKT                                                
         MVC   27(1,R5),29(R2)     STATUS                                       
         SPACE 1                                                                
         SR    R5,R5                                                            
         LA    R4,RMKTELEM         THEN FILL IN THE REST FROM ELEMENT           
PPMKT10  CLI   0(R4),0                                                          
         BE    PPMKTX                                                           
         CLI   0(R4),X'02'                                                      
         BE    PPMKT20                                                          
         BH    PPMKTX                                                           
         CLI   1(R4),0                                                          
         BE    PPMKTX                                                           
PPMKT15  IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     PPMKT10                                                          
         SPACE 1                                                                
         USING RMKTSTEL,R4                                                      
PPMKT20  MVC   0(32,R3),SAVMKT                                                  
         MVC   18(5,R3),RMKTSTAT                                                
         LA    R3,32(R3)                                                        
         B     PPMKT15                                                          
         SPACE 2                                                                
PPMKTX   B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
SAVMKT   DC    XL32'00'                                                         
         EJECT                                                                  
         USING RINVREC,R2                                                       
PPINV    XC    0(32,R3),0(R3)      INVENTORY                                    
         OC    RINVKSRC(3),RINVKSRC                                             
         BNZ   EXIT                NOT A HEADER                                 
         GOTO1 INVPTR,DMCB,(R2),(R3)                                            
PPINV10  CLI   0(R3),0                                                          
         BE    EXIT                                                             
         MVC   27(1,R3),29(R2)     STATUS                                       
         LA    R3,32(R3)                                                        
         B     PPINV10                                                          
         EJECT                                                                  
         USING RPROHDRD,R2                                                      
PPPRO    CLI   RPROKSTY,RPROKSBQ   X'4301'                                      
         BNE   EXIT                                                             
         CLI   RPROR1ST,RPRDSELQ   1ST OF MINIO SET?                            
         BNE   EXIT                NO, MUST OF DONE IT ALREADY                  
***************                                                                 
* X'C301' - SALESPERSON/STATION PASSIVE KEY                                     
***************                                                                 
PPPROPKD USING RPROHDRD,R3                                                      
         XC    0(32,R3),0(R3)                                                   
         MVI   PPPROPKD.RPROPTYP,RPROPTYQ                                       
         MVI   PPPROPKD.RPROPSTY,RPROPSBQ                                       
         MVC   PPPROPKD.RPROPRCD,RPROKRCD                                       
         MVC   PPPROPKD.RPROPCON,RPROKCON                                       
         MVC   PPPROPKD.RPROPPRO,RPROKPRO                                       
         MVC   PPPROPKD.RPROKCTL,RPRORSTA    STATUS, "NOT" STATION              
*                                                                               
         LA    R1,RPROR1ST                                                      
PPPRO10  CLI   0(R1),0             NO SWITCH ELEMENT?                           
         BNE   *+14                                                             
         XC    0(32,R3),0(R3)      NONE, NO PASSIVE FOR THIS                    
         B     EXIT                                                             
*                                                                               
         CLI   0(R1),RPRSWELQ                                                   
         BE    PPPRO15                                                          
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     PPPRO10                                                          
*                                                                               
         USING RPRSWELD,R1                                                      
PPPRO15  MVC   PPPROPKD.RPROPSAL,RPRSWSAL                                       
         MVC   PPPROPKD.RPROPSTA,RPRSWSTA                                       
*                                                                               
         LA    R3,32(R3)                                                        
***************                                                                 
* X'C302' - OFFICE/SALESPERSON PASSIVE KEY                                      
***************                                                                 
         XC    0(32,R3),0(R3)                                                   
         MVI   PPPROPKD.RPROOTYP,RPROOTYQ                                       
         MVI   PPPROPKD.RPROOSTY,RPROOSBQ                                       
         MVC   PPPROPKD.RPROORCD,RPROKRCD                                       
         MVC   PPPROPKD.RPROOCON,RPROKCON                                       
         MVC   PPPROPKD.RPROOPRO,RPROKPRO                                       
         MVC   PPPROPKD.RPROKCTL,RPRORSTA    STATUS, "NOT" STATION              
*                                                                               
         MVC   PPPROPKD.RPROOOFF,RPRSWOFF                                       
         MVC   PPPROPKD.RPROOSAL,RPRSWSAL                                       
         DROP  R1                                                               
*                                                                               
         LA    R3,32(R3)                                                        
*                                                                               
         B     EXIT                                                             
         DROP  PPPROPKD                                                         
         EJECT                                                                  
       ++INCLUDE REDARSTAT                                                      
         EJECT                                                                  
         PRINT GEN                                                              
       ++INCLUDE REINVPTR                                                       
         PRINT NOGEN                                                            
         EJECT                                                                  
SPACES   DC    CL8'        '                                                    
POWERCDS DC    128C' '             STORAGE FOR MASTER/SUB CODES                 
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
WRKD     DSECT                                                                  
WRKDS    EQU   *                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
HALF     DS    H                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
TODAY    DS    CL8                                                              
DARTYPE  DS    X                   PASSIVE KEY TYPE FOR DARE                    
MASTFIND DS    CL1                 Y = MASTER N = NOT MASTER                    
ALTDUB   DS    CL3                 STATUS WORK AREA STORAGE                     
MASTCODE DS    CL2                 MASTER CODE FOR SUBREP                       
SAVER1   DS    F                                                                
ADAR0A   DS    A                                                                
WRKDX    EQU   *                                                                
WRKDLEN  EQU   WRKDX-WRKDS                                                      
         EJECT                                                                  
RREP     DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
STAD     DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
ADVD     DSECT                                                                  
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
SALD     DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
PRDD     DSECT                                                                  
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
AGYD     DSECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
AGY2D    DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
         EJECT                                                                  
BUYD     DSECT                                                                  
       ++INCLUDE REGENBUY                                                       
         EJECT                                                                  
COND     DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
INVD     DSECT                                                                  
       ++INCLUDE REGENINV                                                       
         EJECT                                                                  
MKGOOD   DSECT                                                                  
       ++INCLUDE REGENMKG                                                       
         EJECT                                                                  
MKTD     DSECT                                                                  
       ++INCLUDE REGENMKT                                                       
         EJECT                                                                  
DARE     DSECT                                                                  
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
       ++INCLUDE REGENPRO                                                       
**PAN#1  CSECT                                                                  
         END                                                                    
