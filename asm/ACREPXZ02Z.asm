*          DATA SET ACREPXZ02Z AT LEVEL 057 AS OF 05/01/02                      
*PHASE ACZX02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE CLPACK                                                                 
*INCLUDE PUBVAL                                                                 
         TITLE 'CLEARANCE UPDATE FIX'                                           
*****************************************************************               
*  DO NOT DELETE                                                *               
*****************************************************************               
ACZX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZX**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZXD,RC                                                         
*-------------------------------------------------------------------*           
*        FIRST FOR RUN                                                          
*-------------------------------------------------------------------*           
         MVI   FCRESET,C'Y'                                                     
         CLI   MODE,RUNFRST                                                     
         BNE   LDGF00                                                           
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         ZAP   TOTDR,=P'0'                                                      
         ZAP   TOTCR,=P'0'                                                      
*                                                                               
         OPEN  (SPOTDATA,OUTPUT)   OPEN SEQUENTIAL DATA SET                     
         LTR   RF,RF               FOR OUTPUT                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (NETDATA,OUTPUT)    OPEN SEQUENTIAL DATA SET                     
         LTR   RF,RF               FOR OUTPUT                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (PRNTDATA,OUTPUT)   OPEN SEQUENTIAL DATA SET                     
         LTR   RF,RF               FOR OUTPUT                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FILNME,CONFIL                                                    
*                                                                               
         MVC   DUB,=CL8'T00A7A'    STAPACK                                      
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    DMCB+4,DMCB+4                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAPACK,4(R1)                                                   
*                                                                               
         BAS   RE,BLDACC           BUILD TABLE OF ALPHA ID/SE#                  
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        FIRST FOR LEDGER                                                       
*-------------------------------------------------------------------*           
LDGF00   CLI   MODE,LEDGFRST                                                    
         BNE   PRAC00                                                           
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'Y'                                                    
         ZAP   LEGDR,=P'0'                                                      
         ZAP   LEGCR,=P'0'                                                      
         ZAP   CHAREC,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCACC                                                                
*-------------------------------------------------------------------*           
PRAC00   CLI   MODE,PROCACC                                                     
         BNE   PTRN00                                                           
*        L     R1,ADHEIRB                                                       
*        CLC   =C'PN924',2(R1)                                                  
*        BNE   PC1                                                              
*        B     PC1                                                              
PC1      ZAP   ACDR,=P'0'                                                       
         ZAP   ACCR,=P'0'                                                       
         MVI   ACTSW,C'N'                                                       
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESS TRANSACTIONS                                                   
*-------------------------------------------------------------------*           
PTRN00   CLI   MODE,PROCTRNS                                                    
         BNE   ACCL00                                                           
         ZAP   TNDR,=P'0'                                                       
         ZAP   TNCR,=P'0'                                                       
         L     R5,ADTRANS                                                       
         USING TRANSD,R5                                                        
*                                                                               
         CLC   =C'SJ',1(R5)        SKIP SJ LEDGER                               
         BE    XIT                                                              
         CLC   =C'SI',1(R5)        SKIP SI LEDGER                               
         BE    XIT                                                              
         CLC   =C'SK',1(R5)        SKIP SK LEDGER                               
         BE    XIT                                                              
*                                                                               
         LR    R3,R5                                                            
         SH    R3,DATADISP                                                      
         USING ACKEYD,R3           R3 BEG OF RECORD                             
         LA    R7,TNDR                                                          
         TM    TRNSSTAT,X'80'                                                   
         BNO   XIT                                                              
         ZAP   0(L'TNDR,R7),TRNSAMNT                                            
         EDIT  TNDR,(15,P+64),2,CR=YES                                          
         EDIT  TNCR,(15,P+87),2,CR=YES                                          
         MVC   P+1(14),ACKEYACC+1                                               
         MVC   P+17(14),ACKEYCON+1                                              
         MVC   P+33(6),TRNSREF                                                  
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(0,P+42)                                
         MVC   P+49(2),TRNSANAL                                                 
         MVC   P+54(6),TRNSBTCH                                                 
         EDIT  (B1,TRNSTYPE),(2,P+62)                                           
*                                                                               
PTRN29   DS    0H                                                               
         BAS   RE,PRINTIT                                                       
         AP    CHAREC,=P'1'                                                     
         MVI   ACTSW,C'Y'                                                       
         AP    ACDR,TNDR                                                        
         AP    ACCR,TNCR                                                        
         BAS   RE,DMPGET                                                        
         BAS   RE,GETCR            GET CREDIT RECORD                            
         BNE   XIT                                                              
         BAS   RE,SETREC                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ACCOUNT LAST                                                           
*-------------------------------------------------------------------*           
ACCL00   CLI   MODE,ACCLAST                                                     
         BNE   LEG00                                                            
         CLI   ACTSW,C'Y'                                                       
         BNE   XIT                                                              
         AP    LEGDR,ACDR                                                       
         AP    LEGCR,ACCR                                                       
         EDIT  ACDR,(15,P+64),2,CR=YES                                          
         EDIT  ACCR,(15,P+87),2,CR=YES                                          
         MVC   P(13),=C'ACCOUNT TOTAL'                                          
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        LAST FOR LEDGER                                                        
*-------------------------------------------------------------------*           
LEG00    CLI   MODE,LEDGLAST                                                    
         BNE   RUNL00                                                           
         AP    TOTDR,LEGDR                                                      
         AP    TOTCR,LEGCR                                                      
         L     R3,ADACC                                                         
         USING ACKEYD,R3                                                        
         BAS   RE,PRINTIT                                                       
         MVC   P(7),=C'LEDGER '                                                 
         MVC   P+8(1),ACKEYACC+2                                                
         MVC   P+10(6),=C'TOTAL '                                               
         EDIT  LEGDR,(15,P+64),2,CR=YES                                         
         EDIT  LEGCR,(15,P+87),2,CR=YES                                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         BAS   RE,PRINTIT                                                       
         EDIT  (P6,CHAREC),(14,P+20),MINUS=YES                                  
         MVC   P+1(16),=C'CHANGED RECORDS '                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        RUNLAST                                                                
*-------------------------------------------------------------------*           
RUNL00   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         BAS   RE,PRINTIT                                                       
         MVC   P+1(5),=C'TOTAL'                                                 
         EDIT  TOTDR,(15,P+64),2,CR=YES                                         
         EDIT  TOTCR,(15,P+87),2,CR=YES                                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         BAS   RE,PUTFILE          GET SORT RECORDS & PUT THEM TO FILE          
         CLI   SORTFRST,C'Y'                                                    
         BE    RUNL10                                                           
         GOTO1 ADSORTER,DMCB,=C'END'    END SORT                                
*                                                                               
RUNL10   CLOSE SPOTDATA            CLOSE SEQUENTIAL DATA SET                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE NETDATA             CLOSE SEQUENTIAL DATA SET                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE PRNTDATA            CLOSE SEQUENTIAL DATA SET                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         EDIT  COUNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                         
         MVC   P+15(30),=CL30'RECORDS'                                          
         BAS   RE,PRINTIT                                                       
*                                                                               
         EDIT  SPOTCNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+15(30),=CL30'SPOT RECORDS'                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
         EDIT  NETCNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                        
         MVC   P+15(30),=CL30'NET RECORDS'                                      
         BAS   RE,PRINTIT                                                       
*                                                                               
         EDIT  PRNTCNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                       
         MVC   P+15(30),=CL30'PRINT RECORDS'                                    
         BAS   RE,PRINTIT                                                       
*                                                                               
         EDIT  BADCNT,(12,P+2),0,COMMAS=YES,ZERO=NOBLANK                        
         MVC   P+15(30),=CL30'BAD RECORDS'                                      
         BAS   RE,PRINTIT                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
YES      CR    RB,RB                                                            
         B     XIT                                                              
NO       LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        GET CREDIT RECORD                                                      
*                                                                               
         USING TRNRECD,R3          R3=A(TRANSACTION KEY)                        
GETCR    NTR1                                                                   
         USING MPYELD,R6           R6=A(MANUAL PAYMENT ELEMENT)                 
         L     R6,ADTRANS                                                       
         MVI   ELCODE,MPYELQ       X'64'                                        
         BAS   RE,NEXTEL                                                        
         BNE   GCNO                                                             
         CLI   MPYLN,MPYLNQ                                                     
         BNH   GCNO                                                             
         MVC   DRSEQ,MPYSUB        DEBIT SHOULD HAVE SEQ NUM OF CREDIT          
         DROP  R6                                                               
*                                                                               
         XC    ACKEY,ACKEY                                                      
         MVC   ACKEY(41),0(R3)           SET UP THE SAME KEY                    
         MVC   ACKEY+41(1),DRSEQ         SEQ NUMBER OF MATCHING CREDIT          
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',ACKEY,IO,0                       
*                                                                               
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'ACCMST ',ACCKDA,IO,WORK               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ TRANSACTION                       
         DROP  R3                                                               
*                                                                               
         LA    R3,IO                                                            
         USING TRNRECD,R3                                                       
         CLC   ACKEY(42),TRNKEY                                                 
         BE    YES                                                              
*                                                                               
GCNO     AF    BADCNT,=F'1'        INCREMENT COUNTER                            
         MVC   P(22),=C'** ERROR ** NO CREDIT '                                 
***      GOTO1 HEXOUT,DMCB,0(R3),P+25,40,=C'TOG'                                
         BAS   RE,PRINTIT                                                       
         B     NO                                                               
         DROP  R3                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ROUTINE TO SET UP RECORDS TO BE PROCESSED                              
*-------------------------------------------------------------------*           
         USING TRNRECD,R3          R3=A(TRANSACTION KEY)                        
SETREC   NTR1                                                                   
         LA    R3,IO                                                            
*                                                                               
         MVI   SYSSW,C' '                                                       
         LA    R4,SREC                                                          
         USING SRECD,R4                                                         
         XC    SREC,SREC           CLEAR RECORD                                 
         CLI   TRNKUNT,C'S'        ALL VENDORS = C'S'                           
         BNE   PROCX                                                            
*                                                                               
         CLI   TRNKLDG,C'P'        PRINT VENDORS C'SP'                          
         BE    PROC30                                                           
         CLI   TRNKLDG,C'Q'        CANCADIAN PRINT VENDORS C'SQ'                
         BE    PROC30                                                           
         CLI   TRNKLDG,C'U'        NET VENDORS C'SU'                            
         BE    PROC20                                                           
         CLI   TRNKLDG,C'S'        SPOT & NET VENDORS CAN BE C'SS'              
         BE    PROC17                                                           
         CLI   TRNKLDG,C'T'        CANADIAN SPOT                                
         BNE   PROCX                                                            
*                                                                               
PROC17   LA    R6,TRNRFST                                                       
         MVI   ELCODE,TRNELQ                                                    
         USING TRNELD,R6           R6=A(TRANSACTION ELEMENT)                    
         BAS   RE,FIRSTEL                                                       
         BNE   PROCX                                                            
*                                                                               
         BAS   RE,ISSPOT           CHECK IF SPOT (CC EQ) OR NET (NEQ)           
         BNE   PROC20                                                           
         MVI   SYSSW,C'S'                                                       
         BAS   RE,PROCSPOT         PROCESS SPOT CLEARANCES                      
         B     PROCX                                                            
*                                                                               
PROC20   MVI   SYSSW,C'N'                                                       
         BAS   RE,PROCSPOT         PROCESS NET CLEARANCES                       
         B     PROCX                                                            
*                                                                               
PROC30   MVI   SYSSW,C'P'                                                       
         BAS   RE,PROCPRNT         PROCESS PRINT CLEARANCES                     
*                                                                               
PROCX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        IF THIS IS A SPOT CLEARANCE RETURN CC EQ                               
*        IF THIS IS A NET CLEARANCE RETURN CC NEQ                               
*                                                                               
         USING SRECD,R4                                                         
ISSPOT   NTR1                                                                   
         LA    R6,TRNRFST                                                       
         MVI   ELCODE,TRNELQ                                                    
         USING TRNELD,R6           R6=A(TRANSACTION ELEMENT)                    
         BAS   RE,FIRSTEL                                                       
         BE    *+6                 MUST HAVE ELEMENT                            
         DC    H'0'                                                             
*                                                                               
         MVC   WORK,SPACES                                                      
         CLI   TRNTYPE,X'22'       IF THIS'S A REP - INFO IN CONTRA ACC         
         BNE   IS10                                                             
         MVC   SMED,TRNKCACT       MEDIA                                        
         B     IS20                                                             
*                                                                               
IS10     MVC   SMED,TRNKACT        ELSE INFO IN ACCOUNT - MEDIA                 
*                                                                               
IS20     CLI   SMED,C'N'           MEDIA 'N' - A NET CLEARANCE                  
         BE    NO                                                               
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SPOT CLEARANCES                                                
*        INPUT - R3 = REC                                                       
*        INPUT - R4 = SREC                                                      
*                                                                               
         USING TRNRECD,R3          R3=A(TRANSACTION KEY)                        
         USING SRECD,R4                                                         
PROCSPOT NTR1                                                                   
         BAS   RE,CKREC            SEE IF RECORD IS NEEDED                      
         BNE   PRSPX                                                            
*                                                                               
         LA    R6,TRNRFST                                                       
         MVI   ELCODE,TRNELQ                                                    
         USING TRNELD,R6           R6=A(TRANSACTION ELEMENT)                    
         BAS   RE,FIRSTEL                                                       
         BNE   PRSPX               MUST HAVE ELEMENT                            
*                                                                               
         MVC   SSTA,SPACES                                                      
         CLI   TRNTYPE,X'22'       IF THIS'S A REP - INFO IN CONTRA ACC         
         BNE   PRSP10                                                           
         MVC   SMED,TRNKCACT       MEDIA                                        
         MVC   SSTA,TRNKCACT+1     CALL LETTERS                                 
         B     PRSP20                                                           
*                                                                               
PRSP10   MVC   SMED,TRNKACT        ELSE INFO IN ACCOUNT - MEDIA                 
         MVC   SSTA,TRNKACT+1      CALL LETTERS                                 
*                                                                               
PRSP20   DS    0H                                                               
         CLI   SMED,C'T'           IF MEDIA IS 'T'                              
         BNE   *+8                                                              
         MVI   SSTA+4,C'T'         MUST SET IT FOR MSPACK                       
*                                                                               
         GOTO1 =V(CLPACK),DMCB,TRNKCACT+9,SCLT                                  
*                                                                               
         BAS   RE,SORTPUT                                                       
         CLI   SYSSW,C'N'                                                       
         BE    PRSP60                                                           
         AF    SPOTCNT,=F'1'       INCREMENT COUNTER                            
         B     PRSPX                                                            
PRSP60   AF    NETCNT,=F'1'        INCREMENT COUNTER                            
*                                                                               
PRSPX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS NET CLEARANCES                                                 
*        INPUT - R3 = REC                                                       
*        INPUT - R4 = SREC                                                      
*                                                                               
         USING TRNRECD,R3          R3=A(TRANSACTION KEY)                        
         USING SRECD,R4                                                         
PROCNET  NTR1                                                                   
         BAS   RE,CKREC            SEE IF RECORD IS NEEDED                      
         BNE   PRNTX               MUST HAVE ELEMENT                            
*                                                                               
         LA    R6,TRNRFST                                                       
         MVI   ELCODE,TRNELQ                                                    
         USING TRNELD,R6           R6=A(TRANSACTION ELEMENT)                    
         BAS   RE,FIRSTEL                                                       
         BNE   PRNTX               MUST HAVE ELEMENT                            
*                                                                               
         MVC   WORK,SPACES                                                      
         CLI   TRNTYPE,X'22'       IF THIS'S A REP - INFO IN CONTRA ACC         
         BNE   PRNT10                                                           
         MVC   SMED,TRNKCACT       MEDIA                                        
         MVC   WORK(5),TRNKCACT+1  CALL LETTERS                                 
         B     PRNT20                                                           
*                                                                               
PRNT10   MVC   SMED,TRNKACT        ELSE INFO IN ACCOUNT - MEDIA                 
         MVC   WORK(5),TRNKACT+1   CALL LETTERS                                 
*                                                                               
PRNT20   CLI   SMED,C'N'           THIS MUST BE NET (MEDIA MUST BE N)           
         BNE   PRNTX                                                            
*                                                                               
*        GOTO1 =V(MSPACK),DMCB,=C'0000',WORK,WORK+10                            
*        CLI   0(R1),X'FF'                                                      
*        BNE   PRNT30                                                           
*        BAS   RE,STAERR                                                        
*        B     PRNTX                                                            
*                                                                               
PRNT30   MVC   SSTA,WORK+12        WE DON'T HAVE MARKET YET                     
         GOTO1 =V(CLPACK),DMCB,TRNKCACT+9,SCLT                                  
*                                                                               
         BAS   RE,SORTPUT                                                       
         AF    NETCNT,=F'1'        INCREMENT COUNTER                            
*                                                                               
PRNTX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS PRINT CLEARANCES                                               
*        INPUT - R3 = REC                                                       
*        INPUT - R4 = SREC                                                      
*                                                                               
         USING TRNRECD,R3          R3=A(TRANSACTION KEY)                        
         USING SRECD,R4                                                         
PROCPRNT NTR1                                                                   
         BAS   RE,CKREC            SEE IF RECORD IS NEEDED                      
         BNE   PRPRX                                                            
         LA    R6,TRNRFST                                                       
         MVI   ELCODE,TRNELQ                                                    
         USING TRNELD,R6           R6=A(TRANSACTION ELEMENT)                    
*                                                                               
         BAS   RE,FIRSTEL                                                       
         BNE   PRPRX                                                            
         MVC   SPCLT,TRNKCACT+9                                                 
         MVC   WORK,SPACES                                                      
         CLI   TRNTYPE,X'32'       IF THIS'S A REP - INFO IN CONTRA ACC         
         BNE   PRPR10                                                           
         MVC   SMED,TRNKCCPY       MEDIA                                        
         MVC   WORK(8),TRNKCUNT    PUB                                          
         B     PRPR20                                                           
*                                                                               
PRPR10   MVC   SMED,TRNKACT        MEDIA                                        
         MVC   WORK(8),TRNKACT+1   PUB                                          
*                                                                               
PRPR20   GOTO1 =V(PUBVAL),DMCB,WORK,SPUB                                        
*                                                                               
         BAS   RE,SORTPUT                                                       
         AF    PRNTCNT,=F'1'       INCREMENT COUNTER                            
*                                                                               
PRPRX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CHECK IF RECORD IS NEEDED                                              
*        INPUT - R3 = REC                                                       
*        INPUT - R4 = SREC                                                      
*                                                                               
         USING TRNRECD,R3          R3=A(TRANSACTION KEY)                        
         USING SRECD,R4                                                         
CKREC    NTR1                                                                   
         LA    R6,TRNRFST                                                       
         MVI   ELCODE,TRNELQ       X'44'                                        
         USING TRNELD,R6           R6=A(TRANSACTION ELEMENT)                    
         BAS   RE,FIRSTEL                                                       
         BNE   CRNO                MUST HAVE ELEMENT                            
*                                                                               
         TM    TRNSTAT,TRNSBREC    IF RECORD WAS RECONCILED                     
         BNO   *+8                                                              
         OI    SSTATUS,TRNSBREC    TURN ON BIT IN RECORD                        
*                                                                               
         BAS   RE,GETINFO          GET XPY/MPY FROM CREDIT REC                  
         BNE   CRNO                                                             
*                                                                               
CRYES    MVC   SKSYS,SYSSW         SYSTEM - S/P/N                               
         B     YES                                                              
*                                                                               
CRNO     AF    BADCNT,=F'1'        INCREMENT COUNTER                            
         MVC   P(22),=C'** ERROR ** BAD RECORD'                                 
***      GOTO1 HEXOUT,DMCB,0(R3),P+25,40,=C'TOG'                                
         BAS   RE,PRINTIT                                                       
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
*        GET XPY & MPY ELEMENTS & SET SE NUMBER AND ALPHA ID                    
*        R3 = RECORD                                                            
***********************************************************************         
         USING TRNRECD,R3                                                       
GETINFO  NTR1                                                                   
         LA    R6,TRNRFST                                                       
         MVI   ELCODE,XPYELQ                                                    
         USING XPYELD,R6           R6=A(MANUAL PAYMENT ELEMENT)                 
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   GINO                                                             
*                                                                               
         OC    XPYAGY,XPYAGY                                                    
         BZ    GINO                                                             
*                                                                               
         MVC   SKALPHA,XPYAGY      ALPHA AGENCY                                 
         MVC   SSEQNUM,XPYSEQ      SEQUENCE NUMBER                              
         BAS   RE,GETSE            GET SE NUM                                   
         MVC   SKSENUM,BYTE                                                     
*                                                                               
*        NOW GET MPY INFO                                                       
*                                                                               
         LA    R6,TRNRFST                                                       
         MVI   ELCODE,MPYELQ       X'64'                                        
         USING MPYELD,R6           R6=A(MANUAL PAYMENT ELEMENT)                 
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   GINO                                                             
         MVC   SCKNUM,MPYNO        CHECK NUMBER                                 
         MVC   SCKDATE,MPYDTE      DATE OF CHECK                                
*                                                                               
*&&DO                                                                           
         LA    R6,TRNRFST                                                       
         MVI   ELCODE,GDAELQ                                                    
         USING GDAELD,R6           R6=A(GENERAL DATE ELEMENT)                   
*                                                                               
GI10     BAS   RE,NEXTEL           USE GENERAL DATE ELEMENT IF                  
         BNE   GI20                IT'S FOUND                                   
         CLI   GDATYPE,GDATCLR     IS THIS A CLEARANCE DATE                     
         BNE   GI10                                                             
         GOTO1 DATCON,DMCB,(1,GDADATE),(2,SCKCDT)                               
         B     GIYES                                                            
*&&                                                                             
*                                                                               
GI20     LA    R6,TRNRFST                                                       
         MVI   ELCODE,TRSELQ                                                    
         USING TRSELD,R6           R6=A(TRANSACTION STATUS ELEMENT)             
         BAS   RE,NEXTEL                                                        
         BNE   *+10                                                             
         MVC   SCKCDT,TRSDATE      CLEARANCE DATE                               
*                                                                               
GIYES    B     YES                                                              
*                                                                               
GINO     B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* READ ACCESS RECORDS & BUILD A TABLE OF ALPHA ID/SE#                 *         
***********************************************************************         
BLDACC   NTR1                                                                   
         XR    R4,R4               COUNTER                                      
         LA    R3,IDTAB            TABLE OF ALPHA ID/SE NUM                     
         USING IDTABD,R3                                                        
*                                                                               
         LA    R6,KEY              BUILD ACCESS KEY                             
         USING CT5REC,R6                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    ACCESS RECORD CODE                           
         BAS   RE,HIGH                                                          
         B     BA20                                                             
*                                                                               
BA10     BAS   RE,SEQ                                                           
*                                                                               
BA20     LA    R6,IO                                                            
         CLI   0(R6),CT5KTYPQ      STILL ACCESS RECORD                          
         BNE   BAX                                                              
         MVC   IDALPHA,CT5KALPH    ALPHA ID                                     
*                                                                               
         LA    R6,CT5DATA          1ST ELEMENT                                  
         USING CTSYSD,R6                                                        
         MVI   ELCODE,CTSYSELQ     SYSTEM ELEMENT X'21'                         
         CLI   CTSYSEL,CTSYSELQ                                                 
         BE    BA35                                                             
*                                                                               
BA30     BAS   RE,NEXTEL                                                        
         BNE   BA50                                                             
*                                                                               
BA35     CLI   CTSYSNUM,2          FIND SPOT ELEMENT                            
         BNE   BA40                                                             
         MVC   IDSPSEN,CTSYSSE     SET SE NUMBER                                
         B     BA30                & GET NEXT ELEMENT                           
*                                                                               
BA40     CLI   CTSYSNUM,3          FIND NET ELEMENT                             
         BNE   BA45                                                             
         MVC   IDNESEN,CTSYSSE     SET SE NUMBER                                
         B     BA30                & GET NEXT ELEMENT                           
*                                                                               
BA45     CLI   CTSYSNUM,4          FIND PRINT ELEMENT                           
         BNE   BA30                                                             
         MVC   IDPRSEN,CTSYSSE     SET SE NUMBER                                
*                                                                               
BA50     MVI   IDCTRY,C'U'                                                      
         LA    R6,IO                                                            
         USING CT5REC,R6                                                        
         LA    R6,CT5DATA          1ST ELEMENT                                  
         USING CTAGDD,R6                                                        
         MVI   ELCODE,CTAGDELQ     AGENCY GROUP DETAILS                         
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                DIE                                          
         TM    CTAGDCTY,X'08'      IS THIS CANADA                               
         BNO   BA60                                                             
         MVI   IDCTRY,C'C'                                                      
*                                                                               
BA60     OC    0(IDDLNQ,R3),0(R3)  WAS ANYTHING INPUT                           
         BZ    BA70                                                             
         LA    R4,1(R4)            INCREMENT COUNTER                            
         LA    R3,IDDLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
         C     R3,=A(IDTABX)       IF REACHED END OF TABLE                      
         BL    *+6                                                              
         DC    H'0'                DIE                                          
*                                                                               
BA70     B     BA10                GET NEXT RECORD                              
*                                                                               
BAX      B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*        FIND SE NUMBER CORRESPONDING TO ALPHA AGENCY ID *                      
*        RETURN SE NUMBER IN BYTE *                                             
***********************************************************************         
         SPACE                                                                  
GETSE    NTR1                                                                   
         LA    R3,IDTAB                                                         
         USING IDTABD,R3                                                        
*                                                                               
GS10     C     R3,=A(IDTABX)       IF REACHED END OF TABLE                      
         BNL   GSDIE                                                            
         OC    IDALPHA,IDALPHA     OR NO MORE ENTRIES                           
         BZ    GSDIE                                                            
         CLC   IDALPHA,SKALPHA     OR WENT PAST ENTRY                           
         BH    GSDIE               DIE                                          
         BE    GS20                IF FOUND - DONE                              
         LA    R3,IDDLNQ(R3)       ELSE BUMP TO NEXT ENTRY                      
         B     GS10                                                             
*                                                                               
GS20     MVC   BYTE,IDSPSEN        SET SPOT BYTE                                
         CLI   SYSSW,C'S'          CHECK WHICH SYSTEM WE'RE RUNNING             
         BE    GSX                                                              
         MVC   BYTE,IDNESEN        SET NET BYTE                                 
         CLI   SYSSW,C'N'          CHECK WHICH SYSTEM WE'RE RUNNING             
         BE    GSX                                                              
         MVC   BYTE,IDPRSEN        SET PRINT BYTE                               
*                                                                               
GSX      MVC   THISCTRY,IDCTRY                                                  
         B     XIT                                                              
*                                                                               
GSDIE    DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*              READ CONTROL FILE                                                
***********************************************************************         
         SPACE                                                                  
HIGH     NTR1                                                                   
         MVC   COMMAND,=CL8'DMRDHI'                                             
         MVC   KEYSAVE,KEY                                                      
         B     GTFLE                                                            
*                                                                               
SEQ      NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
         MVC   KEYSAVE,KEY                                                      
*                                                                               
GTFLE    GOTO1 DATAMGR,DMCB,(0,COMMAND),FILNME,KEY,IO,0                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              SORT UTILITIES                                                   
*                                                                               
         SPACE                                                                  
SORTPUT  NTR1                                                                   
         CLI   SORTFRST,C'Y'                                                    
         BNE   SORTPUT2                                                         
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
         MVI   SORTFRST,C'N'                                                    
*                                                                               
SORTPUT2 GOTO1 ADSORTER,DMCB,=C'PUT',SREC                                       
****     GOTO1 HEXOUT,DMCB,SREC,P,25,=C'TOG'                                    
         BAS   RE,PRINTIT                                                       
*                                                                               
SPX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        WRITE RECORDS TO FILE                                                  
*                                                                               
PUTFILE  NTR1                                                                   
         CLI   SORTFRST,C'Y'                                                    
         BE    PFX                                                              
*                                                                               
PF10     GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,DMCB+4                                                        
         LTR   R2,R2                                                            
         BZ    PFX                                                              
*                                                                               
         USING SRECD,R2                                                         
*        CLC   LASTSORT,0(R2)      IF SAME EXACT REC BUMP SEQ NUMBER            
*        BNE   PF14                                                             
*        ZIC   R1,LASTSEQ                                                       
*        LA    R1,1(R1)                                                         
*        MVC   LASTSORT,0(R2)      SAVE ORIGINAL IN CASE THERE ARE MORE         
*        STC   R1,SSEQNUM          THEN UPDATE SEQ NUMBER                       
*        MVC   LASTSEQ,SSEQNUM     AND SAVE                                     
*        B     PF16                                                             
*                                                                               
*F14     MVC   LASTSORT,0(R2)      SAVE INFO                                    
*        MVC   LASTSEQ,SSEQNUM                                                  
         DROP  R2                                                               
*                                                                               
PF16     LA    R3,SPOTDATA                                                      
         CLI   0(R2),C'S'                                                       
         BE    PF20                                                             
         LA    R3,NETDATA                                                       
         CLI   0(R2),C'N'                                                       
         BE    PF20                                                             
         LA    R3,PRNTDATA                                                      
         CLI   0(R2),C'P'                                                       
         BE    PF20                                                             
         DC    H'0'                                                             
*                                                                               
PF20     MVC   WORK(SRECLNQ-1),1(R2)                                            
         PUT   (R3),WORK           PUT RECORD TO SEQUENTIAL DATA SET            
         AF    COUNT,=F'1'         INCREMENT COUNTER                            
         B     PF10                                                             
*                                                                               
PFX      B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ROUTINES TO DUMP OUT RECORDS                                           
*-------------------------------------------------------------------*           
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
*****    SR    R8,R8                                                            
*****    ICM   R8,3,ACLENGTH                                                    
*****    GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              ROUTINE TO GET AN ELEMENT                                        
*                                                                               
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*-------------------------------------------------------------------*           
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',ACCOUNT),((R4),(R2)),((R5),(R3))                
         B     XIT                                                              
*-------------------------------------------------------------------*           
*              ROUTINE TO ADD AN ELEMENT                                        
*                                                                               
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
*-------------------------------------------------------------------*           
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCOUNT),(R2),(R3)                              
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        PRINT OUT ERROR MESSAGE                                                
*                                                                               
         SPACE                                                                  
STAERR   NTR1                                                                   
         MVC   P(25),=CL25'*** STATION ERROR ***'                               
         MVC   P+27(5),WORK                                                     
         BAS   RE,PRINTIT                                                       
         B     XIT                                                              
         SPACE 2                                                                
PRINTIT  NTR1                                                                   
         GOTO1 ACREPORT                                                         
         MVI   SPACING,1                                                        
         B     XIT                                                              
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        LITERALS                                                               
*-------------------------------------------------------------------*           
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
         SPACE                                                                  
CONFIL   DC    C'CTFILE  '                                                      
*                                                                               
SPOTDATA DCB   DDNAME=SPOTDATA,DSORG=PS,MACRF=PM,EODAD=PFX,            X        
               RECFM=FB,LRECL=25,BLKSIZE=250                                    
*                                                                               
NETDATA  DCB   DDNAME=NETDATA,DSORG=PS,MACRF=PM,EODAD=PFX,             X        
               RECFM=FB,LRECL=25,BLKSIZE=250                                    
*                                                                               
PRNTDATA DCB   DDNAME=PRNTDATA,DSORG=PS,MACRF=PM,EODAD=PFX,            X        
               RECFM=FB,LRECL=25,BLKSIZE=250                                    
*                                                                               
         DS    0F                                                               
SORTFRST DC    C'Y'                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,14,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(26)'                                  
*                                                                               
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
*                                                                               
TNDR     DC    PL6'0'                                                           
TNCR     DC    PL6'0'                                                           
*                                                                               
CHAREC   DC    PL6'0'                                                           
*                                                                               
ACDR     DC    PL6'0'                                                           
ACCR     DC    PL6'0'                                                           
*                                                                               
LEGDR    DC    PL6'0'                                                           
LEGCR    DC    PL6'0'                                                           
*                                                                               
TOTDR    DC    PL6'0'                                                           
TOTCR    DC    PL6'0'                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'50'                                                          
*                                                                               
*-------------------------------------------------------------------*           
*        ID TABLE                                                               
*-------------------------------------------------------------------*           
         SPACE                                                                  
         DS    0D                                                               
         DC    CL8'*IDTAB**'                                                    
IDTAB    DS    1200CL(IDDLNQ)      ALPHA ID/SE # TABLE                          
IDTABX   EQU   *                                                                
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        STORAGE                                                                
*-------------------------------------------------------------------*           
         SPACE                                                                  
ACZXD    DSECT                                                                  
PARM     DS    6F                                                               
VSTAPACK DS    A                                                                
TODAY2   DS    CL2                                                              
DMPSW    DS    CL1                                                              
ACTSW    DS    CL1                                                              
SYSSW    DS    CL1                                                              
SREC     DS    CL(SRECLNQ)                                                      
ELCODE   DS    CL1                                                              
COMMAND  DS    CL8                                                              
FILNME   DS    CL8                                                              
ACKEY    DS    CL56                                                             
DRSEQ    DS    XL1                                                              
*                                                                               
THISCTRY DS    CL1                 COUNTRY                                      
*                                                                               
COUNT    DS    F                   COUNT OF OUTPUT RECORDS                      
SPOTCNT  DS    F                   COUNTER OF SPOT RECORDS                      
NETCNT   DS    F                   NET                                          
PRNTCNT  DS    F                   PRINT                                        
BADCNT   DS    F                   BAD RECORDS                                  
LASTSORT DS    CL(SRECLNQ)                                                      
LASTSEQ  DS    XL1                                                              
*                                                                               
IO       DS    CL1000                                                           
IOB      DS    CL1000                                                           
         EJECT                                                                  
SRECD    DSECT                                                                  
SKEY     DS    0C                  SORT KEY                                     
SKSYS    DS    CL1                 SYSTEM                                       
SKSENUM  DS    XL1                 SE NUMBER                                    
SKALPHA  DS    CL2                 ALPHA ID                                     
SMED     DS    CL1                 MEDIA CODE                                   
SCLT     DS    XL2                 CLIENT                                       
SSTA     DS    CL5                 STATION                                      
         DS    CL2                                                              
         ORG   SCLT                                                             
SPCLT    DS    CL3                 PRINT CLIENT                                 
SPUB     DS    CL6                 PUB                                          
SKEYLNQ  EQU   *-SKEY              LENGTH OF KEY                                
*                                                                               
SCKCDT   DS    XL2                 CHECK CLEARANCE DATE                         
SSEQNUM  DS    XL1                 SEQUENCE NUM                                 
SCKNUM   DS    CL6                 CHECK NUMBER                                 
SCKDATE  DS    XL2                 CHECK DATE                                   
SSTATUS  DS    XL1                 STATUS                                       
SRECLNQ  EQU   *-SRECD             LENGTH OF SORT RECORD                        
         SPACE 3                                                                
*                                                                               
IDTABD   DSECT                     ALPHA ID/SE # TABLE DSECT                    
IDALPHA  DS    CL2                 ALPHA ID                                     
IDCTRY   DS    CL1                 COUNTRY CODE                                 
IDSPSEN  DS    XL1                 SPOT SE NUMBER                               
IDNESEN  DS    XL1                 NET SE NUMBER                                
IDPRSEN  DS    XL1                 PRINT SE NUMBER                              
IDDLNQ   EQU   *-IDTABD                                                         
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
*  CTGENFILE                                                                    
*  SPSTAPACKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057ACREPXZ02Z05/01/02'                                      
         END                                                                    
