*          DATA SET REREPOW02  AT LEVEL 009 AS OF 08/31/00                      
*          DATA SET REREPOW02  AT LEVEL 008 AS OF 11/05/98                      
*PHASE REOW02A                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'MODULE TO PURGE INACTIVE OWNERS'                                
*                                                                               
*******************************************************************             
*                                                                 *             
*        REREP2402 (RE2402) --- REP FILE MARKER                   *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 31JUL98 (JRD) INITIAL ENTRY                                     *             
*                                                                 *             
* QUESTOR = $$PRINT WILL PRINT BEFORE/AFTER RECORD DUMPS          *             
*                                                                 *             
* QOPTION1 = S  = DRILL TO STATION LEVEL(DEFAULT)                 *             
*            C  = DRILL TO CONTRACT LEVEL                         *             
*                                                                 *             
* QOPTION2 = T  = TEST:  NO PUTRECS                               *             
*            U  = HARD UPDATE                                     *             
*                                                                 *             
* --------------------------------------------------------------- *             
*                                                                 *             
*******************************************************************             
*                                                                               
REOW02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REOW02,R9,RR=R5                                              
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         L     RE,ADCONLST                                                      
         L     RE,VCOMFACS-ADCONSD(RE)                                          
         MVC   VHEXOUT,CHEXOUT-COMFACSD(RE)                                     
*                                                                               
         LA    RE,*                                                             
         AHI   RE,IO1-(*-4)                                                     
         ST    RE,AIO1                                                          
         LA    RE,*                                                             
         AHI   RE,IO2-(*-4)                                                     
         ST    RE,AIO2                                                          
         LA    RE,*                                                             
         AHI   RE,IO3-(*-4)                                                     
         ST    RE,AIO3                                                          
         MVC   AIOAREA,AIO1                                                     
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   *+14                                                             
         MVC   MASTREP,RREPKREP                                                 
         B     EXITOK                                                           
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXITH                                                            
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
         GOTO1 REPORT                                                           
*                                                                               
*----------------------*                                                        
* READ REP RECORD      *                                                        
*----------------------*                                                        
         XC    REPSTBL,REPSTBL     CLEAR REPCODE LIST                           
*                                                                               
         XC    KEY,KEY             READ REP RECORD AND BUILD REPSTBL            
K        USING RREPKEY,KEY                                                      
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,MASTREP                                               
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GREC                                                             
*                                                                               
         L     R6,AIOAREA                                                       
R        USING RREPREC,R6                                                       
         CLC   R.RREPMAST,=X'FFFF'   MASTER REP?                                
         BE    *+14                YES                                          
         MVC   REPSTBL(2),MASTREP                                               
         B     RDREP100                                                         
         DROP  R                                                                
*                                                                               
         LA    R6,RREPELEM-RREPREC(R6)                                          
RDREP002 DS    0H                                                               
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'02'                                                      
         BE    RDREP004                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RDREP002                                                         
*                                                                               
RDREP004 DS    0H                                                               
E        USING RREPSUB,R6                                                       
         ZIC   RF,E.RREPSCNT                                                    
         LA    R6,E.RREPSCOD                                                    
         DROP  E                                                                
*                                                                               
         LA    R3,REPSTBL                                                       
RDREP010 DS    0H                                                               
         LA    R0,REPSTBL+L'REPSTBL                                             
         CR    R3,R0               CHECK END OF TABLE                           
         BL    RDREP012                                                         
*                                                                               
         MVC   P(L'REPSERR),REPSERR                                             
         GOTO1 REPORT                                                           
         B     EXITOK                                                           
*                                                                               
RDREP012 DS    0H                                                               
         MVC   0(2,R3),0(R6)                                                    
         LA    R6,2(R6)                                                         
         LA    R3,2(R3)                                                         
         BCT   RF,RDREP010                                                      
*                                                                               
RDREP100 DS    0H                                                               
         ZAP   OWNCOUNT,=P'0'                                                   
         EJECT                                                                  
*                                                                               
*--------------------*                                                          
* READ OWNER RECORDS *                                                          
*--------------------*                                                          
         XC    KEY,KEY                                                          
K        USING ROWNKEY,KEY                                                      
         MVI   K.ROWNKTYP,X'2A'                                                 
         MVC   K.ROWNKREP,MASTREP  OWNERS AT MASTER LEVEL                       
         MVC   KEYSAVE,KEY                                                      
         DROP  K                                                                
         GOTO1 HIGH                                                             
*                                                                               
RDOWN010 DS    0H                                                               
         CLC   KEY(ROWNKOWN-ROWNKEY),KEYSAVE                                    
         BNE   RDRECX                                                           
*                                                                               
         MVC   OWNKEY,KEY          SAVE THIS KEY                                
*                                                                               
         GOTO1 GREC                                                             
*                                                                               
         L     R6,AIOAREA                                                       
         USING ROWNREC,R6                                                       
         MVC   OWNNAME,ROWNNAME                                                 
         DROP  R6                                                               
*                                                                               
*----------------------*                                                        
* READ STATION RECORDS *                                                        
*----------------------*                                                        
         LA    RE,REPSTBL          STATIONS ARE AT THE SUBSIDAIRY LVL           
         ST    RE,ANXTREP                                                       
RDSTA002 DS    0H                                                               
         L     R1,ANXTREP           READ THE NEXT REPCODE                       
         LA    R0,REPSTBL+L'REPSTBL                                             
         CR    R1,R0                                                            
         BNL   OWNINACT            OWNER IS INACTIVE                            
*                                                                               
         MVC   REPCODE,0(R1)                                                    
         LA    R1,2(R1)                                                         
         ST    R1,ANXTREP                                                       
*                                                                               
         OC    REPCODE,REPCODE                                                  
         BZ    OWNINACT            OWNER IS INACTIVE                            
*                                                                               
         XC    KEY,KEY                                                          
K        USING RST5KEY,KEY                                                      
         MVI   K.RST5KTYP,X'83'                                                 
         MVI   K.RST5KSTP,X'03'                                                 
         MVC   K.RST5KREP,REPCODE                                               
         MVC   K.RST5KOWN,OWNKEY+(ROWNKOWN-ROWNKEY)                             
         MVC   KEYSAVE,KEY                                                      
         DROP  K                                                                
         GOTO1 HIGH                                                             
*                                                                               
RDSTA010 DS    0H                                                               
         CLC   KEY(RST5KSTA-RST5KEY),KEYSAVE                                    
         BNE   RDSTA002             NO MORE STATIONS FOR THIS OWN/REP           
*                                                                               
         MVC   STAKEY,KEY                                                       
*                                                                               
         CLI   QOPTION1,C'C'       CONTRACT LEVEL OPTION?                       
         BNE   OWNACT               NO - OWNER IS ACTIVE                        
*                                                                               
*-----------------------*                                                       
* READ CONTRACT RECORDS *                                                       
*-----------------------*                                                       
         XC    KEY,KEY                                                          
K        USING RCONKEY,KEY                                                      
         MVI   K.RCONSTYP,X'CC'                                                 
         MVC   K.RCONSREP,REPCODE                                               
         MVC   K.RCONSSTA,STAKEY+(RST5KSTA-RST5KEY)                             
         MVC   KEYSAVE,KEY                                                      
         DROP  K                                                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(RCONSOFF-RCONKEY),KEYSAVE                                    
         BE    OWNACT               OWNER IS ACTIVE                             
*                                                                               
*--------------*                                                                
* NEXT STATION *                                                                
*--------------*                                                                
         MVC   KEY,STAKEY          RESTORE STATION SEQUENCE                     
         GOTO1 HIGH                                                             
         CLC   STAKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SEQ                                                              
         B     RDSTA010                                                         
*                                                                               
*----------------*                                                              
* INACTIVE OWNER *                                                              
*----------------*                                                              
OWNINACT DS    0H                                                               
         MVC   P(08),=C'INACTIVE'                                               
         MVC   P+10(L'ROWNKOWN),OWNKEY+(ROWNKOWN-ROWNKEY)                       
         MVC   P+15(L'OWNNAME),OWNNAME                                          
         GOTO1 REPORT                                                           
*                                                                               
         AP    OWNCOUNT,=P'1'                                                   
*                                                                               
         CLI   QOPTION1,C'C'       CONTRACT LEVEL OPTION?                       
         BNE   OIA0060             NO = SKIP STATION CHANGES                    
*                                                                               
         LA    RE,REPSTBL          STATIONS ARE AT THE SUBSIDAIRY LVL           
         ST    RE,ANXTREP                                                       
OIA0002  DS    0H                                                               
         L     R1,ANXTREP           READ THE NEXT REPCODE                       
         LA    R0,REPSTBL+L'REPSTBL                                             
         CR    R1,R0                                                            
         BNL   OIA0050                                                          
*                                                                               
         MVC   REPCODE,0(R1)                                                    
         LA    R1,2(R1)                                                         
         ST    R1,ANXTREP                                                       
*                                                                               
         OC    REPCODE,REPCODE                                                  
         BZ    OIA0050                                                          
*                                                                               
         CLI   P+45,C' '                                                        
         BNH   OIA0004                                                          
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
OIA0004  DS    0H                                                               
         MVC   P+40(2),REPCODE                                                  
         LA    R5,P+45                                                          
*                                                                               
         XC    KEY,KEY             PURGE OWNER CODE FROM STATION                
K        USING RST5KEY,KEY                                                      
         MVI   K.RST5KTYP,X'83'                                                 
         MVI   K.RST5KSTP,X'03'                                                 
         MVC   K.RST5KREP,REPCODE                                               
         MVC   K.RST5KOWN,OWNKEY+(ROWNKOWN-ROWNKEY)                             
         MVC   KEYSAVE,KEY                                                      
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
OIA0010  DS    0H                                                               
         CLC   KEY(RST5KSTA-RST5KEY),KEYSAVE                                    
         BNE   OIA0002             NO MORE STATIONS FOR THIS OWN/REP            
*                                                                               
         MVC   STAKEY,KEY                                                       
*                                                                               
         MVC   0(L'RSTAKSTA,R5),KEY+(RST5KSTA-RST5KEY)                          
         LA    R5,L'RSTAKSTA+1(R5)                                              
         LA    R0,P+L'P-L'RSTAKSTA+1  END OF LINE PRINT IT                      
         CR    R5,R0                                                            
         BL    OIA0014                                                          
*                                                                               
         GOTO1 REPORT                                                           
         LA    R5,P+45                                                          
*                                                                               
OIA0014  DS    0H                                                               
         GOTO1 GREC                                                             
         L     R6,AIOAREA                                                       
         USING RSTAREC,R6                                                       
*                                                                               
         XC    WORK,WORK                                                        
E        USING RSTAHOEL,WORK                                                    
         MVI   E.RSTAHOEC,RSTAHOEQ                                              
         MVI   E.RSTAHOLN,RSTAHOLQ                                              
         MVC   E.RSTAHOWN,RSTAOWN                                               
         GOTO1 DATCON,DMCB,(5,0),(19,E.RSTAHODT)                                
         DROP  E,R6                                                             
*                                                                               
         LA    R6,RSTAELEM-RSTAREC(R6) FIND ELEMENT A HOME                      
OIA0020  DS    0H                                                               
         CLI   0(R6),0             EOR?                                         
         BE    OIA0022             YES                                          
         CLI   0(R6),RSTAHOEQ                                                   
         BNL   OIA0022                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     OIA0020                                                          
*                                                                               
OIA0022  DS    0H                                                               
         CLC   =C'$$PRINT',QUESTOR                                              
         BNE   OIA0024                                                          
*                                                                               
         MVC   P(07),=C'BEFORE:'                                                
         GOTO1 REPORT                                                           
         L     R1,AIOAREA                                                       
         GOTO1 PRNTREC                                                          
*                                                                               
OIA0024  DS    0H                                                               
         GOTO1 =V(RECUP),DMCB,(C'R',AIOAREA),WORK,(R6)                          
*                                                                               
         CLC   =C'$$PRINT',QUESTOR                                              
         BNE   OIA0026                                                          
*                                                                               
         MVC   P(06),=C'AFTER:'                                                 
         GOTO1 REPORT                                                           
         L     R1,AIOAREA                                                       
         GOTO1 PRNTREC                                                          
*                                                                               
OIA0026  DS    0H                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    OIA0030             YES                                          
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   OIA0030             NO                                           
*                                                                               
         GOTO1 PREC                                                             
*                                                                               
OIA0030  DS    0H                                                               
         XC    KEY,KEY                                                          
         L     R6,AIOAREA                                                       
K        USING RST6KEY,KEY                                                      
         MVI   K.RST6KTYP,X'83'                    SET RECORD ID                
         MVI   K.RST6KSTP,X'04'                    SET RECORD SUBID             
         MVC   K.RST6KREP,RSTAKREP-RSTAREC(R6)     SET REP CODE                 
         MVC   K.RST6KOWN,RSTAOWN-RSTAREC(R6)      SET OWNER NAME               
         MVC   K.RST6KSTA,RSTAKSTA-RSTAREC(R6)     SET STATION CALL             
         LA    RF,RSTAELEM-RSTAREC(R6)                                          
OIA0032  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    OIA0036             YES                                          
         CLI   0(RF),X'08'         EXTENDED DESCRIPTION ELEMENT?                
         BE    OIA0034             YES                                          
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     OIA0032                                                          
*                                                                               
OIA0034  DS    0H                                                               
         MVC   K.RST6KMKT,RSTAMKTC-RSTAXXEL(RF)                                 
         DROP  K                                                                
*                                                                               
OIA0036  DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OIA0038                                                          
*                                                                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
*                                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    OIA0038             YES                                          
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   OIA0038             NO                                           
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
OIA0038  DS    0H                                                               
         MVC   KEY,STAKEY          RESTORE STATION SEQUENCE                     
         GOTO1 HIGH                                                             
         CLC   STAKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+L'RSTAKEY,X'80'  DELETE KEY                                  
*                                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    OIA0042             YES                                          
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   OIA0042             NO                                           
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
OIA0042  DS    0H                                                               
         GOTO1 SEQ                                                              
         B     OIA0010                                                          
*                                                                               
OIA0050  DS    0H                                                               
         CLI   P+45,C' '                                                        
         BNH   OIA0052                                                          
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
OIA0052  DS    0H                                                               
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
*                                                                               
OIA0060  DS    0H                                                               
         MVC   KEY,OWNKEY          RESTORE OWNER SEQUENCE                       
         GOTO1 HIGH                                                             
         CLC   OWNKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+L'ROWNKEY,X'80'  DELETE KEY                                  
*                                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    OIA0062             YES                                          
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   OIA0062             NO                                           
*                                                                               
         GOTO1 WRITE                                                            
*                                                                               
OIA0062  DS    0H                                                               
*                                                                               
         GOTO1 GREC                DELETE OWNER RECORD                          
*                                                                               
         CLC   =C'$$PRINT',QUESTOR                                              
         BNE   OIA0064                                                          
*                                                                               
         MVC   P(07),=C'BEFORE:'                                                
         GOTO1 REPORT                                                           
         L     R1,AIOAREA                                                       
         GOTO1 PRNTREC                                                          
*                                                                               
OIA0064  DS    0H                                                               
         L     R6,AIOAREA                                                       
         OI    (ROWNLEN-ROWNKEY)+2(R6),X'80'                                    
*                                                                               
         CLC   =C'$$PRINT',QUESTOR                                              
         BNE   OIA0066                                                          
*                                                                               
         MVC   P(06),=C'AFTER:'                                                 
         GOTO1 REPORT                                                           
         L     R1,AIOAREA                                                       
         GOTO1 PRNTREC                                                          
*                                                                               
OIA0066  DS    0H                                                               
         CLI   QOPTION2,C'T'       TEST RUN?                                    
         BE    OIA0100             YES                                          
         CLI   QOPTION2,C'U'       UPDATIVE?                                    
         BNE   OIA0100             NO                                           
*                                                                               
         GOTO1 PREC                                                             
*                                                                               
OIA0100  DS    0H                                                               
         B     RDOWNNXT                                                         
*                                                                               
*--------------*                                                                
* ACTIVE OWNER *                                                                
*--------------*                                                                
OWNACT   DS    0H                                                               
         B     OWNA10              DON'T PRINT ACTIVE                           
*                                                                               
         MVC   P(07),=C'*ACTIVE'                                                
         MVC   P+10(L'ROWNKOWN),OWNKEY+(ROWNKOWN-ROWNKEY)                       
         MVC   P+15(L'OWNNAME),OWNNAME                                          
         MVC   P+40(2),REPCODE                                                  
         MVC   P+45(L'RSTAKSTA),STAKEY+(RSTAKSTA-RSTAKEY)                       
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
OWNA10   DS    0H                                                               
         MVC   KEY,OWNKEY          RESTORE OWNER SEQUENCE                       
         GOTO1 HIGH                                                             
         CLC   OWNKEY,KEY                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RDOWNNXT DS    0H                                                               
         GOTO1 SEQ                                                              
         B     RDOWN010                                                         
*                                                                               
RDRECX   DS    0H       123456789.123456789.123456789.                          
         GOTO1 REPORT                                                           
         MVI   P,C'-'                                                           
         MVC   P+1(79),P                                                        
         GOTO1 REPORT                                                           
         MVC   P(21),=C'TOTAL OWNERS PURGED: '                                  
         EDIT  (P5,OWNCOUNT),(17,P+26),ZERO=NOBLANK,ALIGN=LEFT                  
         GOTO1 REPORT                                                           
         B     EXITOK                                                           
*                                                                               
EXITH    LTR   RB,RB                                                            
         B     EXIT                                                             
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
EXIT     DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
***********************************************************************         
* PRINT A REP RECORD FOR DEBUGGING                                              
***********************************************************************         
PRNTREC  NTR1                                                                   
         LR    R6,R1                                                            
         MVC   P(12),=C'RECORD DUMP:'                                           
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 HEXOUT,DMCB,0(R6),P+2,34                                         
         GOTO1 REPORT                                                           
*                                                                               
         LA    R6,34(R6)           FIRST ELEMENT                                
PRNTR10  DS    0H                                                               
         CLI   0(R6),0                                                          
         BE    PRNTR20                                                          
*                                                                               
         ZIC   R2,1(R6)                                                         
         LA    R4,0(R6)                                                         
         LA    R0,P+5                                                           
PRNTR12  DS    0H                                                               
         LR    R3,R2                                                            
         CH    R3,=H'60'                                                        
         BNH   *+8                                                              
         LA    R3,60                                                            
*                                                                               
         SR    R2,R3                                                            
         GOTO1 HEXOUT,DMCB,(R4),(R0),(R3)                                       
         GOTO1 REPORT                                                           
*                                                                               
         AR    R4,R3                                                            
         LA    R0,P+10                                                          
         LTR   R2,R2                                                            
         BNZ   PRNTR12                                                          
*                                                                               
         ZIC   R2,1(R6)                                                         
         LA    R6,0(R2,R6)                                                      
         B     PRNTR10                                                          
*                                                                               
PRNTR20  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         DS    0D                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
*                                                                               
REPSERR  DC    C'TOO MANY SUBSIDIARY REPS, CONTACT DDS'                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         TITLE 'MODULE WORKING STORAGE'                                         
RELO     DC    F'0'                                                             
VHEXOUT  DS    A                                                                
AIOAREA  DS    A                                                                
AIO1     DS    A                                                                
AIO2     DS    A                                                                
AIO3     DS    A                                                                
COMMAND  DS    CL8                                                              
*                                                                               
OWNCOUNT DC    PL5'0'                                                           
*                                                                               
MASTREP  DS    CL2                                                              
*                                                                               
ANXTREP  DS    A                   ADRESS OF NEXT ENTRY IN REPSTBL              
REPCODE  DS    CL2                 REPCODE                                      
REPNAME  DS    CL33                                                             
REPSTBL  DC    XL(30*2)'00'        TABLE OF REPCODES CODES TO RUN               
*                                                                               
OWNKEY   DS    CL27                                                             
STAKEY   DS    CL27                                                             
*                                                                               
OWNNAME  DS    CL20                                                             
*                                                                               
IO1      DS    4096X                                                            
IO2      DS    4096X                                                            
IO3      DS    4096X                                                            
         EJECT                                                                  
*  INCLUDE REGENALL                                                             
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*  INCLUDE DDCOMFACS                                                            
         PRINT OFF                                                              
       ++INCLUDE REGENALL                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REREPOW02 08/31/00'                                      
         END                                                                    
