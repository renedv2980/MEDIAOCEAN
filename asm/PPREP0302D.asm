*          DATA SET PPREP0302D AT LEVEL 003 AS OF 05/01/02                      
*PHASE PP0302D,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*      THIS PROGRAM CREATES PUB ADDRESS RECORDS (RECORD CODE X'82')             
*      FROM THE ADDRESS ELEMENTS OF EXISTING PUB RECORDS (CODE X'81')           
*                                                                               
*                                                                               
*      QOPT1     Y= DELETE ADDRESS ELEMENTS FROM PUB RECORD                     
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 25 RECORDS                                       
*      QOPT7     Y= DUMP ANY PUB RECORD OVER 3500 BYTES                         
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCPUB                                                     
         BE    PROC                                                             
         CLI   MODE,FPUBREQ                                                     
         BE    RUNF                                                             
         CLI   MODE,LPUBREQ                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   DUMPADD,=P'0'                                                    
         ZAP   DUMPDEL,=P'0'                                                    
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNF5    CLI   0(R4),X'FF'                                                      
         BE    RUNFX                                                            
         ZAP   0(8,R4),=P'0'                                                    
         LA    R4,23(R4)                                                        
         B     RUNF5                                                            
RUNFX    B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         GOTO1 HIGHPUB                                                          
         CLI   KEY+9,X'81'         PUB REC ?                                    
         BNE   PROC5               NO - BYPASS                                  
         TM    KEY+25,X'01'        PASSIVE ?                                    
         BO    PROC5               YES - BYPASS                                 
         GOTO1 GETNAME                                                          
         B     PROC5A                                                           
*                                                                               
PROC5    DS    0H                                                               
         CLI   KEY,X'FF'           EOF ?                                        
         BE    EXIT                YES                                          
         BAS   RE,NXTPUB           GET PUB REC                                  
*                                                                               
PROC5A   MVI   DUMPSW,0                                                         
         MVI   ADDSW,0                                                          
         MVI   DELSW,0                                                          
         CLI   PUBKMED,X'FF'       EOF ?                                        
         BNE   PROC5A2             NO                                           
         MVI   KEY,X'FF'                                                        
         MVI   PAGYKMED,X'FF'      TO FORCE PPG TO END                          
         B     EXIT                                                             
PROC5A2  AP    INCNT,=P'1'                                                      
         LA    R4,ALLCNTS                                                       
         ZICM  R5,PUBREC+25,2      PUB REC LENGTH                               
         BAS   RE,SIZEPUBS         COUNT PUB RECS BY SIZE                       
         CLI   QOPT7,C'Y'          DUMP "BIG" RECORDS ?                         
         BNE   PROC5A4             NO                                           
         CH    R5,=H'3500'         BIG ENOUGH TO DUMP ?                         
         BL    PROC5A4             NO                                           
         BAS   RE,RPRT                                                          
         MVC   P+30(24),=C'*** BIG RECORD BELOW ***'                            
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
PROC5A4  MVC   SAVKEY,KEY          SAVE KEY OF PUB REC                          
*                             *******************************                   
*                             ******  ADD PUB ADDRESS RECORDS                   
         LA    R2,PUBREC+33   *******************************                   
         MVI   ELCODE,X'08'                                                     
PROC5B   BAS   RE,NEXTEL           LOOK FOR PAY ADDRESS ELEM                    
         BNE   PROC5D              NOT FOUND                                    
         BAS   RE,OUTREC           ADD A PUB ADDRESS RECORD                     
         CLC   OUTSPC+11(3),=X'FFFFFF'   AGENCY ?                               
         BNE   PRB2                NO                                           
         AP    PAYACNT,=P'1'                                                    
         B     PROC5BX                                                          
PRB2     CLI   OUTSPC+11,X'FF'     OFFICE ?                                     
         BNE   PRB4                NO                                           
         AP    PAYOCNT,=P'1'                                                    
         B     PROC5BX                                                          
PRB4     AP    PAYCCNT,=P'1'       MUST BE CLIENT                               
PROC5BX  B     PROC5B              LOOK FOR ANOTHER                             
*                                                                               
PROC5D   LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'09'                                                     
PROC5D2  BAS   RE,NEXTEL           LOOK FOR TRAFFIC ADDRESS ELEM                
         BNE   PROC5G              NOT FOUND                                    
         BAS   RE,OUTREC           ADD A PUB ADDRESS RECORD                     
         CLC   OUTSPC+11(3),=X'FFFFFF'   AGENCY ?                               
         BNE   PRD2                NO                                           
         AP    TRAACNT,=P'1'                                                    
         B     PROC5DX                                                          
PRD2     CLI   OUTSPC+11,X'FF'     OFFICE ?                                     
         BNE   PRD4                NO                                           
         AP    TRAOCNT,=P'1'                                                    
         B     PROC5DX                                                          
PRD4     AP    TRACCNT,=P'1'       MUST BE CLIENT                               
PROC5DX  B     PROC5D2             LOOK FOR ANOTHER                             
*                                                                               
PROC5G   LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'0A'                                                     
PROC5G2  BAS   RE,NEXTEL           LOOK FOR CONTRACT ADDRESS ELEM               
         BNE   PROC5K              NOT FOUND                                    
         BAS   RE,OUTREC           ADD A PUB ADDRESS RECORD                     
         CLC   OUTSPC+11(3),=X'FFFFFF'   AGENCY ?                               
         BNE   PRG2                NO                                           
         AP    CONACNT,=P'1'                                                    
         B     PROC5GX                                                          
PRG2     CLI   OUTSPC+11,X'FF'     OFFICE ?                                     
         BNE   PRG4                NO                                           
         AP    CONOCNT,=P'1'                                                    
         B     PROC5GX                                                          
PRG4     AP    CONCCNT,=P'1'       MUST BE CLIENT                               
PROC5GX  B     PROC5G2             LOOK FOR ANOTHER                             
*                                                                               
PROC5K   LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'0B'                                                     
PROC5K2  BAS   RE,NEXTEL           LOOK FOR SHIPPING ADDRESS ELEM               
         BNE   PROC10              NOT FOUND                                    
         BAS   RE,OUTREC           ADD A PUB ADDRESS RECORD                     
         CLC   OUTSPC+11(3),=X'FFFFFF'   AGENCY ?                               
         BNE   PRK2                NO                                           
         AP    SHPACNT,=P'1'                                                    
         B     PROC5KX                                                          
PRK2     CLI   OUTSPC+11,X'FF'     OFFICE ?                                     
         BNE   PRK4                NO                                           
         AP    SHPOCNT,=P'1'                                                    
         B     PROC5KX                                                          
PRK4     AP    SHPCCNT,=P'1'       MUST BE CLIENT                               
PROC5KX  B     PROC5K2             LOOK FOR ANOTHER                             
*                              ***************************                      
*                              ******  DELETE PUBREC ELEMS                      
PROC10   DS    0H              ***************************                      
*                                                                               
         CLI   ADDSW,1             HAS OUTREC BEEN CALLED ?                     
         BNE   PROC10B             NO - NO NEED TO RESTORE READ SEQ.            
         MVC   KEY,SAVKEY          SAVKEY HAS KEY OF PUB READ                   
         GOTO1 HIGHPUB             RESTORE READ SEQUENCE                        
         CLC   KEY(31),SAVKEY      SAME RECORD ?                                
         BE    *+6                 YES                                          
         DC    H'0'                NO - VERY BAD                                
         GOTO1 GETNAME             READ FOR UPDATE                              
*                                                                               
PROC10B  CLI   QOPT1,C'Y'          DELETE ADDR ELEMS FROM PUB RECORD ?          
         BNE   PROC5               NO - GO TEST NEXT REC                        
         MVI   DUMPSW,0            TO PRINT "BEFORE" RECORD                     
*                                                                               
PROC15B  LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'08'                                                     
         BAS   RE,NEXTEL           LOOK FOR PAY ADDRESS ELEM                    
         BNE   PROC15D             NOT FOUND                                    
         BAS   RE,DELELEM          DELETE THIS ELEMENT                          
         B     PROC15B             LOOK FOR ANOTHER                             
*                                                                               
PROC15D  LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'09'                                                     
         BAS   RE,NEXTEL           LOOK FOR TRAFFIC ADDRESS ELEM                
         BNE   PROC15G             NOT FOUND                                    
         BAS   RE,DELELEM          DELETE THIS ELEMENT                          
         B     PROC15D             LOOK FOR ANOTHER                             
*                                                                               
PROC15G  LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'0A'                                                     
         BAS   RE,NEXTEL           LOOK FOR CONTRACT ADDRESS ELEM               
         BNE   PROC15K             NOT FOUND                                    
         BAS   RE,DELELEM          DELETE THIS ELEMENT                          
         B     PROC15G             LOOK FOR ANOTHER                             
*                                                                               
PROC15K  LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'0B'                                                     
         BAS   RE,NEXTEL           LOOK FOR SHIPPING ADDRESS ELEM               
         BNE   PROC20              NOT FOUND - GO WRITE RECORD                  
         BAS   RE,DELELEM          DELETE THIS ELEMENT                          
         B     PROC15K             LOOK FOR ANOTHER                             
*                                                                               
PROC20   DS    0H                                                               
         CLI   DELSW,1             HAS AN ELEMENT BEEN DELETED ?                
         BNE   PROC5               NO - GO TEST NEXT REC                        
         LA    R4,AFTCNTS          YES - GET THE "AFTER" SIZE                   
         ZICM  R5,PUBREC+25,2      PUB REC LENGTH                               
         BAS   RE,SIZEPUBS         COUNT PUB RECS BY SIZE                       
         BAS   RE,CLEARREC         CLEAR END OF RECORD                          
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC20D             NO                                           
         CP    DUMPDEL,=P'25'      25 RECORDS DUMPED ?                          
         BH    PROC20D             YES                                          
*                                                                               
         MVC   P+50(21),=C'** AFTER DELETIONS **'                               
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,KEY,P+01,31,=C'N'                                    
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC20D  DS    0H                                                               
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC5               NO - GO TEST NEXT REC                        
         GOTO1 PUTPUB                                                           
*                                                                               
         B     PROC5               GET NEXT PUB                                 
         EJECT                                                                  
*                                                                               
OUTREC   DS    0H           ******      ADD PUB ADDRESS RECORD                  
         NTR1               ****** R2 POINTING TO PUB ADDRESS ELEMENT           
         ST    R2,FULL             SAVE POINTER TO ADDRESS ELEM                 
         MVI   ADDSW,1             OUTREC HAS BEEN CALLED                       
         XC    OUTSPC,OUTSPC       BUILD NEW RECORD IN OUTSPC                   
         MVC   OUTSPC(25),PUBREC                                                
         MVI   OUTSPC+9,X'82'      RECORD CODE                                  
         MVC   OUTSPC+10(1),0(R2)  ADDRESS TYPE                                 
         MVC   OUTSPC+11(3),2(R2)  AGENCY OR CLIENT OR OFFICE                   
         LA    R5,33               HEADER LENGTH                                
         ZIC   R6,1(R2)            ELEMENT LENGTH                               
         AR    R5,R6                                                            
         STH   R5,HALF                                                          
         MVC   OUTSPC+25(2),HALF   RECORD LENGTH                                
         BCTR  R6,0                PREP FOR EXECUTED MOVE                       
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   OUTSPC+33(0),0(R2)    EXECUTED                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),OUTSPC                                                   
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),OUTSPC      DOES PUB ADDRESS RECORD EXIST ?              
         BNE   OUTRECB             NO                                           
         AP    DUPCNT,=P'1'                                                     
         BAS   RE,RPRT             YES                                          
         MVC   P(39),=C'** PUB ADDRESS RECORD ALREADY EXISTS **'                
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         B     OUTRECX             RETURN - NO ADD                              
*                                                                               
OUTRECB  AP    ADDCNT,=P'1'                                                     
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   OUTRECD             NO                                           
*                                                                               
         CLI   DUMPSW,1            "BEFORE" DUMPED ?                            
         BE    OUTRECD             YES                                          
         MVI   DUMPSW,1            NO - DO NOW                                  
         AP    DUMPADD,=P'1'                                                    
         CP    DUMPADD,=P'25'      25 RECORDS DUMPED ?                          
         BH    OUTRECD             YES                                          
         BAS   RE,RPRT                                                          
         MVC   KEY,SAVKEY          SAVKEY HAS KEY OF PUB READ                   
         MVC   P+55(12),=C'** BEFORE **'                                        
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
OUTRECD  XC    KEY,KEY                                                          
         MVC   KEY(25),OUTSPC      SET NEW RECORD KEY FOR ADD                   
*                                                                               
         LA    R0,OUTSPC                                                        
         ST    R0,AREC                                                          
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   OUTRECG             NO                                           
         CP    DUMPADD,=P'25'      25 RECORDS DUMPED ?                          
         BH    OUTRECG             YES                                          
*                                                                               
         MVC   P+55(12),=C'** AFTER ***'                                        
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
OUTRECG  DS    0H                                                               
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    OUTRECX             NO                                           
         GOTO1 ADDPUB                                                           
*                                                                               
OUTRECX  DS    0H                                                               
         L     R2,FULL             RESTORE POINTER TO ADDRESS ELEMENT           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
DELELEM  NTR1                ***** DELETE PUB ADDRESS ELEMENT                   
         CLI   DELSW,1             HAS AN ELEMENT BEEN DELETED YET ?            
         BE    DELELB              YES                                          
         LA    R4,BEFCNTS          NO - GET THE "BEFORE" SIZES                  
         ZICM  R5,PUBREC+25,2      PUB REC LENGTH                               
         BAS   RE,SIZEPUBS         COUNT PUB RECS BY SIZE                       
DELELB   CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   DELELD              NO                                           
*                                                                               
         CLI   DUMPSW,1            "BEFORE" DUMPED ?                            
         BE    DELELD              YES                                          
         MVI   DUMPSW,1            NO - DO NOW                                  
*                                                                               
         AP    DUMPDEL,=P'1'                                                    
         CP    DUMPDEL,=P'25'      25 RECORDS DUMPED ?                          
         BH    DELELD              YES                                          
         BAS   RE,RPRT                                                          
         MVC   P+50(22),=C'** BEFORE DELETIONS **'                              
         BAS   RE,RPRT                                                          
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
DELELD   GOTO1 RECUP,DMCB,(1,PUBREC),(R2),0       DELETE ELEMENT                
         AP    ELEMCNT,=P'1'                                                    
         MVI   DELSW,1             AN ELEMENT HAS BEEN DELETED                  
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CLEARREC NTR1                      *****  CLEAR END OF RECORD                   
         SR    R5,R5                                                            
         IC    R5,PUBREC+25                                                     
         SLL   R5,8                                                             
         IC    R5,PUBREC+26                                                     
         SR    RE,RE                                                            
         LA    RE,PUBREC                                                        
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PUBREC                                                        
         LA    RF,2000(RF)                                                      
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         B     EXIT                                                             
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNL10                                                           
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
RUNL10   DS    0H                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
*****    MVI   RCSUBPRG,10                                                      
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*                             **************************                        
***                           COUNT PUB RECS BY SIZE                            
*                                  R4 = COUNTERS ADDRESS                        
*                                  R5 = PUB REC LENGTH                          
*                             **************************                        
SIZEPUBS NTR1                                                                   
         LA    R6,501                                                           
SIZETST  CR    R5,R6               PUB LENGTH LESS ?                            
         BL    SIZEADD             YES                                          
         LA    R4,23(R4)           BUMP TO NEXT COUNTER                         
         LA    R6,500(R6)          NEXT TEST SIZE                               
         CH    R6,=H'4002'         MAXIMUM PUB LENGTH ?                         
         BL    SIZETST             NO                                           
SIZEADD  AP    0(8,R4),=P'1'                                                    
SIZEPUBX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                             READ FOR ALL PUB (X'81') RECORDS                  
NXTPUB   NTR1                                                                   
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
         DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLI   KEY,C'T'            PAST HIGH MEDIA ?                            
         BNH   NP4                 NO - CONTINUE                                
         MVI   KEY,X'FF'           YES - EXIT WITH EOF SET                      
         MVI   PAGYKMED,X'FF'      TO FORCE PPG TO END                          
         B     EXIT                                                             
NP4      CLI   KEY+9,X'81'         PUB REC ?                                    
         BNE   NP2                 NO - BYPASS                                  
         CLI   KEY+25,X'01'        PASSIVE ?                                    
         BE    NP2                 YES - BYPASS                                 
         GOTO1 GETNAME                                                          
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'RECORDS READ'                                               
ADDCNT   DS    PL8                                                              
         DC    CL15'PUBS ADDED'                                                 
ELEMCNT  DS    PL8                                                              
         DC    CL15'ELEMS DELETED'                                              
DUPCNT   DS    PL8                                                              
         DC    CL15'DUPLICATES   '                                              
PAYCCNT  DS    PL8                                                              
         DC    CL15'PAY ADDR - CLT'                                             
PAYOCNT  DS    PL8                                                              
         DC    CL15'         - OFF'                                             
PAYACNT  DS    PL8                                                              
         DC    CL15'         - AGY'                                             
TRACCNT  DS    PL8                                                              
         DC    CL15'TRA ADDR - CLT'                                             
TRAOCNT  DS    PL8                                                              
         DC    CL15'         - OFF'                                             
TRAACNT  DS    PL8                                                              
         DC    CL15'         - AGY'                                             
CONCCNT  DS    PL8                                                              
         DC    CL15'CON ADDR - CLT'                                             
CONOCNT  DS    PL8                                                              
         DC    CL15'         - OFF'                                             
CONACNT  DS    PL8                                                              
         DC    CL15'         - AGY'                                             
SHPCCNT  DS    PL8                                                              
         DC    CL15'SHP ADDR - CLT'                                             
SHPOCNT  DS    PL8                                                              
         DC    CL15'         - OFF'                                             
SHPACNT  DS    PL8                                                              
         DC    CL15'         - AGY'                                             
ALLCNTS  DS    PL8                                                              
         DC    CL15'ALL PUBS LT 501'                                            
         DS    PL8                                                              
         DC    CL15'     501 - 1000'                                            
         DS    PL8                                                              
         DC    CL15'    1001 - 1500'                                            
         DS    PL8                                                              
         DC    CL15'    1501 - 2000'                                            
         DS    PL8                                                              
         DC    CL15'    2001 - 2500'                                            
         DS    PL8                                                              
         DC    CL15'    2501 - 3000'                                            
         DS    PL8                                                              
         DC    CL15'    3001 - 3500'                                            
         DS    PL8                                                              
         DC    CL15'    3501 - 4000'                                            
         DS    PL8                                                              
         DC    CL15'        GT 4000'                                            
BEFCNTS  DS    PL8                                                              
         DC    CL15'BEF PUBS LT 501'                                            
         DS    PL8                                                              
         DC    CL15'     501 - 1000'                                            
         DS    PL8                                                              
         DC    CL15'    1001 - 1500'                                            
         DS    PL8                                                              
         DC    CL15'    1501 - 2000'                                            
         DS    PL8                                                              
         DC    CL15'    2001 - 2500'                                            
         DS    PL8                                                              
         DC    CL15'    2501 - 3000'                                            
         DS    PL8                                                              
         DC    CL15'    3001 - 3500'                                            
         DS    PL8                                                              
         DC    CL15'    3501 - 4000'                                            
         DS    PL8                                                              
         DC    CL15'        GT 4000'                                            
AFTCNTS  DS    PL8                                                              
         DC    CL15'AFT PUBS LT 501'                                            
         DS    PL8                                                              
         DC    CL15'     501 - 1000'                                            
         DS    PL8                                                              
         DC    CL15'    1001 - 1500'                                            
         DS    PL8                                                              
         DC    CL15'    1501 - 2000'                                            
         DS    PL8                                                              
         DC    CL15'    2001 - 2500'                                            
         DS    PL8                                                              
         DC    CL15'    2501 - 3000'                                            
         DS    PL8                                                              
         DC    CL15'    3001 - 3500'                                            
         DS    PL8                                                              
         DC    CL15'    3501 - 4000'                                            
         DS    PL8                                                              
         DC    CL15'        GT 4000'                                            
         DC    X'FF'                                                            
*                                                                               
SAVKEY   DS    CL32                                                             
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ADDSW    DS    CL1                                                              
DELSW    DS    CL1                                                              
DUMPADD  DS    PL5                                                              
DUMPDEL  DS    PL5                                                              
DUMPSW   DS    CL1                                                              
CHKGST   DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
PUBNET   DS    PL8                                                              
PUBCASH  DS    PL8                                                              
PUBMYTAX DS    PL8                                                              
PUBBASIS DS    PL8                                                              
PUBGSTT  DS    PL8                                                              
PUBGSTTP DS    PL8                                                              
*                                                                               
OUTSPC   DS    XL200                                                            
*                                                                               
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREP0302D05/01/02'                                      
         END                                                                    
