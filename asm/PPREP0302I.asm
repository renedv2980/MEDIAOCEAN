*          DATA SET PPREP0302I AT LEVEL 002 AS OF 05/01/02                      
*PHASE PP0302I,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE PUBVAL                                                                 
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM WILL COPY PUB RECORDS FROM ONE PUB NUMBER TO A                 
*   NEW PUB NUMBER.                                                             
*                                                                               
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 10 RECORDS (BEFORE AND AFTER)                    
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302I                                                        
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
         CLI   MODE,FPUBREQ                                                     
         BE    REQF                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
REQF     DS    0H                  FIRST PUB FOR REQUEST                        
         ZAP   INCNT,=P'0'         RECORDS READ                                 
         ZAP   CPYCNT,=P'0'        RECORDS COPIED                               
         ZAP   PUBCNT,=P'0'        PUB RECS COPIED                              
         ZAP   LTLCNT,=P'0'        LTLRECS COPIED                               
         ZAP   ADRCNT,=P'0'        ADDRESS RECS COPIED                          
         ZAP   BADCNT,=P'0'        ??????? RECS COPIED                          
         ZAP   DUMPCNT,=P'0'                                                    
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   ERRSW,C' '          CLEAR ERROR IN NUMBERS TABLE SWITCH          
*                                                                               
         LA    R5,PUBNUMT          TABLE OF OLD AND NEW PUB NUMBERS             
         LA    R6,PUBKEYT          TABLE OF OLD AND NEW PUB KEYS                
         XC    PUBKEYT,PUBKEYT                                                  
*                                                                               
BLDTAB   DS    0H                                                               
         CLI   0(R5),X'FF'         END OF TABLE ?                               
         BNE   BT10                NO                                           
         MVI   0(R6),X'FF'         END OF PUB KEY TABLE                         
         LA    R6,PUBKEYT          POINT TO TABLE START                         
         B     INITA               TEST FOR COPY "VALIDITY"                     
*                                                                               
BT10     MVC   0(1,R6),0(R5)       PUB MEDIA                                    
         SR    R4,R4               CHARACTER COUNTER                            
BT20     CLI   1(R5),C' '          BLANK ?                                      
         BNH   BTVAL               YES - END OF STRING                          
         CH    R4,=H'15'           MAX LENGTH ?                                 
         BE    BTVAL               YES - END OF STRING                          
         LA    R4,1(R4)            ADD TO CHARACTER COUNTER                     
         LA    R5,1(R5)            BUMP TO NEXT NUMBER POS'N                    
         B     BT20                CHECK NEXT CHARACTER                         
*                                                                               
BTVAL    DS    0H                                                               
         SR    R5,R4               POINT R5 TO BEGINNING OF NUMBER              
         GOTO1 =V(PUBVAL),DMCB,((R4),1(R5)),(0,1(R6))   OUTPUT PUB KEY          
         CLI   0(R1),X'FF'         VALID ?                                      
         BNE   *+6                 YES                                          
         DC    H'0'                NO - DIE                                     
         LA    R5,16(R5)           BUMP TO NEXT PUB NUMBER                      
         LA    R6,7(R6)            BUMP TO NEXT PUB KEY                         
         B     BLDTAB              CHECK NEXT NUMBER                            
*                                                                               
BTEND    DS    0H                                                               
*                                                                               
INITA    DS    0H                                                               
         CLI   0(R6),X'FF'         END OF TABLE ?                               
         BE    PROC                YES                                          
*                                                                               
         XC    KEY,KEY             CHECK FOR PUBS FOR "OLD" NUMBERS             
         MVC   KEY(7),0(R6)        MEDIA,PUB,ZONE,EDITION                       
         MVC   KEY+7(2),QAGENCY                                                 
         MVI   KEY+9,X'81'         PUB RECORD CODE                              
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     "OLD" PUB REC FOUND ?                        
         BE    INITC               YES - GO TEST NEW                            
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P(5),=C'PUB# '                                                   
         GOTO1 PUBEDIT,DMCB,KEYSAVE+1,(0,P+5)                                   
         MVC   P+25(19),=C'** REC NOT FOUND **'                                 
         BAS   RE,RPRT                                                          
         MVI   ERRSW,C'Y'          SET ERROR IN NUMBERS TABLE SWITCH            
*                                                                               
INITC    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             CHECK FOR PUBS FOR "NEW" NUMBERS             
         MVC   KEY(7),7(R6)        MEDIA,PUB,ZONE,EDITION                       
         MVC   KEY+7(2),QAGENCY                                                 
         MVI   KEY+9,X'81'         PUB RECORD CODE                              
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     "NEW" PUB REC FOUND ?                        
         BNE   INITE               NO - GO TEST NEXT                            
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P(5),=C'PUB# '                                                   
         GOTO1 PUBEDIT,DMCB,KEYSAVE+1,(0,P+5)                                   
         MVC   P+25(23),=C'** NEW NUMBER EXISTS **'                             
         BAS   RE,RPRT                                                          
         MVI   ERRSW,C'Y'          SET ERROR IN NUMBERS TABLE SWITCH            
*                                                                               
INITE    DS    0H                                                               
         LA    R6,14(R6)                                                        
         B     INITA               NEXT SET OF PUB NUMBERS                      
*                                                                               
PROC     DS    0H                                                               
         CLI   ERRSW,C'Y'          ERROR FOUND ?                                
         BE    EXIT                YES - END THE REQUEST                        
*                                  NO - BEGIN COPY PROCESS                      
         LA    R6,PUBKEYT          TABLE OF OLD AND NEW KEYS                    
*                                                                               
PROC05   DS    0H                                                               
         CLI   0(R6),X'FF'         END OF TABLE ?                               
         BNE   PROC07              NO                                           
         MVI   KEY,X'FF'           YES - COPY DONE                              
         GOTO1 HIGHPUB                                                          
         B     EXIT                                                             
*                                                                               
PROC07   DS    0H                                                               
         XC    KEY,KEY             GET THE PUB FOR "OLD" NUMBER                 
         MVC   KEY(7),0(R6)        MEDIA,PUB,ZONE,EDITION                       
         MVC   KEY+7(2),QAGENCY                                                 
         MVI   KEY+9,X'81'         PUB RECORD CODE                              
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     "OLD" PUB REC FOUND ?                        
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
*                                                                               
PROC10   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(0,GETREC),PUBFILE,KEY+27,PUBREC,DMWORK             
*                                                                               
* TEST RECORD CODE - PRINT "ACTION" MESSAGE(S) ETC.                             
*                                                                               
         AP    INCNT,=P'1'         PUBS READ                                    
*                                                                               
         CLI   PUBKCOD,X'81'       PUB REC ?                                    
         BNE   PROC10D             NO                                           
         GOTO1 PUBEDIT,DMCB,PUBREC+1,(0,P+1)                                    
         GOTO1 PUBEDIT,DMCB,8(R6),(0,P+25)                                      
         BAS   RE,RPRT                                                          
         MVC   P+53(20),PUBNAME                                                 
         MVC   P+110(20),=C'** PUB REC COPIED **'                               
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         AP    PUBCNT,=P'1'         PUBS COPIED                                 
         B     COPY05              COPY THE RECORD                              
*                                                                               
PROC10D  CLI   PUBKCOD,X'85'       LTL REC ?                                    
         BNE   PROC10J             NO                                           
         MVC   P+110(20),=C'** LTL REC COPIED **'                               
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
         AP    LTLCNT,=P'1'         PUBS COPIED                                 
         B     COPY05              COPY THE RECORD                              
*                                                                               
PROC10J  CLI   PUBKCOD,X'82'       ADR REC ?                                    
         BNE   PROC10P             NO                                           
         MVC   P+53(30),PUBREC+38    PUBAONAM                                   
         MVC   P+110(20),=C'** ADR REC COPIED **'                               
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         AP    ADRCNT,=P'1'         PUBS COPIED                                 
         B     COPY05              COPY THE RECORD                              
*                                                                               
PROC10P  MVC   P+110(20),=C'** BAD REC COPIED **'                               
         BAS   RE,RPRT                                                          
         AP    BADCNT,=P'1'         PUBS COPIED                                 
         B     COPY05              COPY THE RECORD                              
*                                                                               
COPY05   DS    0H                  COPY THE PUB RECORD                          
         AP    CPYCNT,=P'1'        PUB COPY COUNT                               
*                                                                               
*                                                                               
*                                  ADD PUB FOR NEW NUMBER                       
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   COPY10              NO - GO ADD                                  
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BNH   DUMPBEF             NO                                           
         B     COPY10              YES - SKIP DUMP                              
*                                                                               
DUMPBEF  BAS   RE,RPRT                                                          
         MVC   P+55(12),=C'** BEFORE **'                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
COPY10   DS    0H                                                               
         BAS   RE,CLEARREC         CLEAR END OF RECORD                          
         MVC   SAVKEY,KEY                                                       
         XC    KEY,KEY                                                          
         MVC   PUBKEY(7),7(R6)    NEW NUMBER                                    
         MVC   KEY(25),PUBREC                                                   
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    COPY12              NO                                           
*                                                                               
         GOTO1 ADDPUB                                                           
*                                                                               
COPY12   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   COPY20              NO                                           
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    COPY20              YES                                          
*                                                                               
         MVC   P+55(12),=C'** AFTER ***'                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
COPY20   DS    0H                  RESTORE READ SEQUENCE                        
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVKEY                                                   
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         GOTO1 SEQPUB              NEXT RECORD                                  
         CLC   KEY(9),KEYSAVE      SAME MED/PUB/AGY ?                           
         BE    PROC10              YES - GO GET RECORD TO COPY                  
         LA    R6,14(R6)           BUMP TO NEXT TABLE ENTRY                     
         B     PROC05              TEST NEXT TABLE ENTRY                        
*                                                                               
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
RUNL10   DS    0H                                                               
*                                                                               
RUNL40   MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNLX                                                            
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL50                                                           
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
CLEARREC NTR1                    CLEAR END OF 4K MAX LENGTH RECORD              
         MVC   HALF,PUBREC+25                                                   
         LH    R1,HALF                                                          
         LA    RE,PUBREC                                                        
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,4000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         B     EXIT                                                             
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,30                                                      
         MVC   HEAD4+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD4+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
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
         MVC   P+85(25),WORK                                                    
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
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'RECORDS READ'                                               
CPYCNT   DS    PL8                                                              
         DC    CL15'RECORDS COPIED'                                             
PUBCNT   DS    PL8                                                              
         DC    CL15'PUB RECS COPIED'                                            
LTLCNT   DS    PL8                                                              
         DC    CL15'LTL RECS COPIED'                                            
ADRCNT   DS    PL8                                                              
         DC    CL15'ADR RECS COPIED'                                            
BADCNT   DS    PL8                                                              
         DC    CL15'??? RECS COPIED'                                            
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
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
OUTSPC   DS    CL40                                                             
*                   MFROM PUB NUM   MTO PUB NUM ....                            
PUBNUMT  DS    0H   ** OLD AND NEW PUB NUMBERS (MEDIA,PUB,ZONE,EDITION)         
         DC    CL32'N999            N1              '                           
         DC    CL32'N987605         N1              '                           
         DC    X'FF'                                                            
*                        *** OLD AND NEW PUB CODES (MEDIA,KEY FORMAT)           
PUBKEYT  DS    CL140      14 BYTES - 7 FROM AND 7 TO    10X (OR MORE)           
*                                                                               
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
DUMPSW   DS    XL1                                                              
LTLSW    DS    XL1                                                              
DELSW    DS    XL1                                                              
REPSW    DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREP0302I05/01/02'                                      
         END                                                                    
