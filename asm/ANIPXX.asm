*          DATA SET ANIPXX     AT LEVEL 015 AS OF 05/01/02                      
*PHASE SPIP02A                                                                  
         TITLE 'SPIP02 - CONVERTS I2 AUTOPAY RECS TO WORKER FILE'               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- INPUT TAPE RECORD                              *         
*                R3 -- OUTPUT WORKER FILE RECORD                      *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- WORK                                           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SPIP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPIP02,R8,RR=R2                                                
         ST    R2,RELO                                                          
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
                                                                                
         CLI   MODE,REQFRST                                                     
         BE    IP10                                                             
EXIT     XIT1                                                                   
RELO     DC    A(0)                                                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
IP10     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TDAY)                                       
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,CTDAY)                                 
         XC    CTDAY,=X'FFFF'       COMPLEMENT                                  
         MVI   WRKRSTAT,0                                                       
         XC    LASTAGY,LASTAGY                                                  
*                                                                               
         CLI   QOPT1,C'T'          TEST SYSTEM                                  
         BNE   IP12                                                             
         CLI   QAREA+49,C'0'       TEST OUTPUT LIMIT                            
         BL    IP12                                                             
         PACK  DUB,QAREA+49(6)                                                  
         CVB   R0,DUB                                                           
         ST    R0,OUTLIMIT                                                      
*                                                                               
         USING APYRECD,R6                                                       
IP12     LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   APYKTYP,APYKTYPQ    X'0D'                                        
         MVI   APYKSUB,APYKSUBQ    X'3A'                                        
         MVC   APYKDATE,CTDAY      TODAY                                        
         GOTO1 HIGH                                                             
         B     IP30                                                             
IP20     GOTO1 SEQ                                                              
IP30     CLC   KEY(APYKAGMD-APYKEY),KEYSAVE    SAME THRU DATE                   
         BNE   IPX                                                              
         LA    R6,KEY                                                           
         MVC   BYTE,APYKAGMD                   SAME AGENCY?                     
         NI    BYTE,X'F0'                                                       
         BAS   RE,CHKAGY           IS AGY IN TABLE                              
         BNE   IP20                                                             
         CLC   BYTE,LASTAGY                                                     
         BE    IP50                                                             
         MVC   LASTAGY,BYTE                                                     
         TM    WRKRSTAT,WSTATOPN   FILE ALREADY OPEN                            
         BNO   *+8                                                              
         BAS   RE,WRKRCLSE         CLOSE LAST AGY                               
         BAS   RE,WRKROPEN         OPEN NEW AGY                                 
*                                                                               
IP50     GOTO1 GETBUY                                                           
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
IP55     CLI   0(R6),0                                                          
         BE    IP20                                                             
         CLI   0(R6),X'01'         ALPHA ELEM                                   
         BE    IP60                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     IP55                                                             
*                                                                               
         USING APYEL,R6                                                         
         USING WRECD,R4                                                         
IP60     LA    R4,IO2              COPY TO WORKER RECORD                        
         XC    0(256,R4),0(R4)                                                  
         MVC   WRPAYER,APYPAYER                                                 
         MVC   WRMEDIA,APYMED                                                   
         MVC   WRCLIENT,APYCLT                                                  
         MVC   WRPRD,APYPRD                                                     
         MVC   WREST,APYEST                                                     
         CLC   =C'ALL',APYEST                                                   
         BE    IP20                SKIP EST = ALL                               
         MVC   WRSTAT(L'APYSTA),APYSTA                                          
         MVC   WRMONTH,APYMONTH                                                 
         MVC   WRINVCE,APYINV                                                   
         MVC   IO2L(2),=Y(WRLENQ)                                               
*                                                                               
         OC    OUTLIMIT,OUTLIMIT   TEST ANY OUTPUT LIMIT                        
         BZ    IP62                                                             
         CLC   OUTLIMIT,SEQNUM                                                  
         BL    IP20                JUST PLOW ON                                 
*                                                                               
IP62     BAS   RE,WRKR                                                          
         LA    R1,350                                                           
         C     R1,SEQNUM           IF AT LEAST 350 WRKR RECS                    
         BH    *+12                OPEN NEW WORKER FILE                         
         BAS   RE,WRKRCLSE                                                      
         BAS   RE,WRKROPEN                                                      
         B     IP20                SEQ                                          
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        EXIT ROUNTINE                                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
IPX      DS    0H                                                               
         BAS   RE,WRKRCLSE                                                      
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                             ERRORS                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
                                                                                
IPERROR  GOTO1 REPORT                                                           
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   CHECK AGENCY TABLE FOR HEX CODE IN BYTE - RETURN ENTRY IN AGYENTRY*         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CHKAGY   NTR1                                                                   
         USING AGYTABD,R3                                                       
         XC    AGYENTRY,AGYENTRY                                                
         LA    R3,AGYTABLE         CLEAR WKFILE BUFFER                          
CA10     CLI   0(R3),X'FF'                                                      
         BE    CAXNO               NOT IN TABLE                                 
         CLC   BYTE,ATAGYHEX       MATCH ON AGY HEX                             
         BNE   CA20                                                             
         CLC   QAGY,ATALPHA        AND MATCH ON AGY ALPHA                       
         BE    CAXYES                                                           
CA20     LA    R3,ATLENQ(R3)                                                    
         B     CA10                                                             
CAXYES   MVC   AGYENTRY,0(R3)      SAVE ENTRY                                   
         SR    RC,RC                                                            
CAXNO    LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   OPEN WORKER FILE                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKROPEN NTR1                                                                   
         L     R0,AWKBUFF          CLEAR WKFILE BUFFER                          
         L     R1,=A(WKBUFFX-WKBUFF)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING AGYTABD,R6                                                       
         LA    R6,AGYENTRY         AGENCY TABLE ENTRY                           
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,ATIDNUM     AGENCY USER ID NUMBER                        
         MVC   WLSYSPRG(3),=C'APY'                                              
         MVC   WLSUBPRG,ATADV      SET FACPAK                                   
         CLI   QOPT1,C'T'          OPTION TO GENERATE ON TST                    
         BNE   *+8                                                              
         MVI   WLSUBPRG,C'T'       SET FACTST                                   
         MVC   WLDAY,TDAY+2                                                     
         MVI   WLCLASS,C'T'        CLASS T FOR WRKF SCRIPTS                     
         MVI   WLTYPE,C'A'         TYPE A FOR IMMEDIATE EXEC                    
         MVC   WLPSWD,SPACES                                                    
         OI    WLATTB,WLATOBJ                                                   
*                                                                               
         LA    R3,IO2                                                           
         MVI   FIXED,C'Y'                                                       
         BAS   RE,WRKR                                                          
*                                                                               
         XC    SEQNUM,SEQNUM                                                    
         XC    WRKRINDX,WRKRINDX                                                
         LA    R5,WRKRINDX                                                      
         USING UKRECD,R5                                                        
         MVC   UKUSRID,ATIDNUM     AGENCY ID NUM                                
         MVC   UKSYSPRG(3),=C'APY'                                              
         MVC   UKFILENO,WLREPRNO   WORKER FILE NUMBER                           
         DROP  R5                                                               
*                                                                               
         MVC   WRKFNO,WLREPRNO     WORKER FILE NUMBER                           
         MVI   FIXED,C'N'                                                       
*                                                                               
         MVC   P(16),=C'WORKER FILE ID ='                                       
         EDIT  (B2,WLUSRID),(4,P+20)                                            
         MVI   P+24,C','                                                        
         MVC   P+25(4),WLFILEID                                                 
         GOTO1 HEXOUT,DMCB,WLDAY,P+29,1,=C'TOG'                                 
         MVC   P+31(1),WLCLASS                                                  
         MVI   P+32,C','                                                        
         EDIT  WLREPRNO,(5,P+33),0,ALIGN=LEFT                                   
         GOTO1 REPORT                                                           
         DROP  R4                                                               
*                                                                               
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   0(4,R1),=F'2101'                                                 
         MVC   4(6,R1),=C'SCRIPT'                                               
         MVC   10(8,R1),=C'SPAUTPAY'                                            
         MVI   18(R1),C'I'           SET TYPE TO INSERT                         
         MVC   30(5,R1),=C'00006'    HEADER LENGTH                              
         MVC   35(5,R1),=C'00100'    INPUT  DATA LENGTH                         
         MVC   40(5,R1),=C'00020'    OUTPUT DATA LENGTH                         
         MVI   45(R1),C'Y'           INSERT ERRORS AT FILE END                  
         MVC   IO2L(2),=H'76'        72 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
*                                                                               
         LA    R1,IO2                                                           
         XC    0(256,R1),0(R1)                                                  
         MVC   10(20,R1),=CL20'SIGN-ON INFORMATION '                            
         MVC   30(8,R1),ATSIGNON                                                
         MVC   38(8,R1),ATPASSWD                                                
         MVC   IO2L(2),=H'50'        46 + 4 BYTES FOR QSAM                      
         BAS   RE,WRKR                                                          
*                                                                               
         OI    WRKRSTAT,WSTATOPN   FILE OPEN                                    
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                   CLOSE WORKER FILE                                 *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKRCLSE NTR1                                                                   
         TM    WRKRSTAT,WSTATOPN         FILE ALREADY OPEN                      
         BNO   WRKRCLX                                                          
         USING WLHDRD,R4                                                        
         LA    R4,IO2                                                           
         XC    0(255,R4),0(R4)           BUILD HEADER                           
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         MVI   FIXED,C'Y'                                                       
         XC    SEQNUM,SEQNUM                                                    
         BAS   RE,WRKR                                                          
         NI    WRKRSTAT,X'FF'-WSTATOPN   FILE NOT OPEN                          
         CLI   QOPT2,C'Y'                OPTION TO SET TO STATUS HOLD           
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,=C'HOLD    ',WRKFILEN,WRKRINDX,IO2,AWKBUFF          
WRKRCLX  B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                    ADD LINE TO WORKER FILE                          *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
WRKR     NTR1                                                                   
         OC    SEQNUM,SEQNUM                                                    
         BZ    WRKR10                                                           
         MVC   IO2(4),=F'2102'                                                  
         EDIT  SEQNUM,(6,IO2+4),0,FILL=0                                        
                                                                                
WRKR10   DS    0H                                                               
         LA    R3,IO2                                                           
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,IO2L                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,DMPRINT,WRKFILE,0,(R3),AWKBUFF                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,SEQNUM                                                        
         LA    R1,1(R1)                                                         
         ST    R1,SEQNUM                                                        
         CLI   QOPT3,C'Y'                                                       
         BNE   EXIT                                                             
         MVC   P,IO2                                                            
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         LTORG                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*   AGENCY TABLE -  ALPHA CODE,SPOT AGY,USERID,PASSWD,ID NUMBER,ADV             
AGYTABLE DC    C'SJ',X'C0',CL8'SJR     ',CL8'        ',XL2'0011',C'1'           
         DC    C'CK',X'B0',CL8'COKEAT  ',CL8'        ',XL2'1131',C'1'           
         DC    C'JW',X'B0',CL8'JWNY    ',CL8'DDS     ',XL2'0016',C'2'           
         DC    X'FF'                                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DMPRINT  DC    CL8'DMPRINT'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
WRKFILEN DC    CL8'WRKFILE'                                                     
AWKBUFF  DC    A(WKBUFF)                                                        
COUNT    DS    F                   NUMBER OF RECORDS PUT OUT                    
OUTLIMIT DS    F                                                                
SEQNUM   DS    F                                                                
WRKFNO   DS    XL2                 WORKER FILE NUMBER                           
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
LASTAGY  DS    XL1                                                              
TDAY     DS    XL3                 YYMMDD PWOS                                  
CTDAY    DS    XL2                 COMPRESSED TODAY                             
DMACTN   DS    CL5                                                              
FIXED    DS    CL1                                                              
WRKRSTAT DS    XL1                                                              
WSTATOPN EQU   X'80'               A WORKER FILE IS OPEN                        
WRKRCMD  DS    CL7                                                              
WRKRINDX DS    CL42                                                             
AGYENTRY DS    XL(ATLENQ)          AGENCY TABLE ENTRY                           
*                                                                               
         DS    0D                                                               
ELEM     DS    CL256                                                            
IO       DS    XL256               IO AREA                                      
IO2L     DS    F                                                                
IO2      DS    XL400               IO AREA                                      
WKBUFF   DS    14336C                                                           
WKBUFFX  EQU   *                                                                
                                                                                
         EJECT                                                                  
***********************************************************************         
*        WORKER FILE DSECT                                                      
***********************************************************************         
WRECD    DSECT                                                                  
WRHDR    DS    CL30                HEADER                                       
WRPAYER  DS    CL12                PAYER                                        
WRMEDIA  DS    CL1                 MEDIA                                        
WRCLIENT DS    CL3                 CLIENT                                       
WRPRD    DS    CL3                 PRODUCT                                      
WREST    DS    CL3                 ESTIMATE                                     
WRSTAT   DS    CL10                STATION                                      
WRMONTH  DS    CL6                 MONTH (MMM/YY)                               
WRINVCE  DS    CL12                INVOICE TRACKING                             
WRLENQ   EQU   *-WRECD                                                          
*                                                                               
***********************************************************************         
*        AGENCY TABLE DSECT                                                     
***********************************************************************         
AGYTABD  DSECT                                                                  
ATALPHA  DS    CL2                 AGENCY ALPHA ID                              
ATAGYHEX DS    XL1                 AGENCY SPOT HEX CODE                         
ATSIGNON DS    CL8                 AGENCY SIGNON ID                             
ATPASSWD DS    CL8                 AGENCY SIGNON PASSWORD                       
ATIDNUM  DS    XL2                 AGENCY USER ID NUMBER                        
ATADV    DS    CL1                 AGENCY'S ADV                                 
ATLENQ   EQU   *-AGYTABD                                                        
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPGENAPAY                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ANIPXX    05/01/02'                                      
         END                                                                    
