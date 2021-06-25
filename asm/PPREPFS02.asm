*          DATA SET PPREPFS02  AT LEVEL 015 AS OF 07/17/96                      
*PHASE PPFS02A,+0,NOAUTO                                                        
*                                                                               
 TITLE 'PPFS02 - CORRECT CHECK#/DATE IN CLEARANCE STATUS RECORDS'               
*                                                                               
* NOTE - THIS PROGRAM CHANGES CHECK NUMBERS/AND OR DATES ACCORDING              
*        TO THE CONTENTS OF AN INTERNAL TABLE (CKTAB). CKTAB MUST               
*        BE MODIFIED AND THE PROGRAM RELINKED FOR EACH RUN.                     
*                                                                               
*        QPAY (CC53) = 6-POSITION (YYMMDD) CHECK CLEARANCE DATE                 
*                       (OPTIONAL)                                              
*                                                                               
*        QOPT1 N= TEST RUN - DON'T MARK FILE                                    
*        QOPT2 Y= DUMP RECORD                                                   
*                                                                               
PPFS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPFS02                                                         
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPFSWRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   CHGCNT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT1,C'N'        MEANS DON'T MARK FILE                          
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    CLRDAT,CLRDAT                                                    
         CLI   QPAY,C' '           CLEARANCE DATE ENTERED ?                     
         BE    PCLT                NO                                           
         GOTO1 DATCON,DMCB,(0,QPAY),(3,CLRDAT)                                  
*                                                                               
PCLT     CLI   QCLIENT,C' '        CLIENT ENTERED ? (REQUIRED)                  
         BNE   PPUB                YES - GO TEST PUB                            
         MVC   P+1(18),=C'CLIENT NOT ENTERED'                                   
         BAS   RE,RPRT             PRINT MSG                                    
         B     EXIT                                                             
*                                                                               
PPUB     DS    0H                                                               
         XC    NEWNUM,NEWNUM                                                    
         CLI   QPUB,C' '           PUB ENTERED ?                                
         BE    MAIN                NO - GO START PROCESSING                     
         GOTO1 PUBVAL,DMCB,(L'QPUB,QPUB),(0,NEWNUM)                             
         CLI   0(R1),X'FF'         INVALID PUB ?                                
         BNE   MAIN                NO - GO START PROCESSING                     
         MVC   P+1(19),=C'PUBLICATION INVALID'                                  
         BAS   RE,RPRT             PRINT REC KEY WITH ABOVE MSG                 
         B     EXIT                                                             
*                                                                               
MAIN     DS    0H                                                               
*                                                                               
         LA    R4,KEY                                                           
         USING PPCLRST,R4          CLEARANCE STATUS RECORD                      
         XC    KEY,KEY                                                          
         MVC   PPCLAGY,QAGENCY      AGENCY                                      
         MVC   PPCLMED,QMEDIA       MEDIA                                       
         MVI   PPCLTYPE,X'25'        REC TYPE                                   
         MVC   PPCLCLT,QCLIENT      CLIENT                                      
         MVC   PPCLPUB,NEWNUM       PUB                                         
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     MAIN10                                                           
*                                                                               
MAIN00   DS    0H                                                               
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN10   CLC   KEY(PPCLPUB-PPCLKEY),KEYSAVE     THRU CLIENT                     
         BNE   EXIT                DONE                                         
         OC    NEWNUM,NEWNUM       PUB ENTERED ?                                
         BZ    MAIN20              NO                                           
         CLC   KEY(PPCLDATE-PPCLKEY),KEYSAVE     THRU PUB                       
         BNE   EXIT                DONE                                         
         DROP  R4                                                               
*                                                                               
MAIN20   DS    0H                  GET CLARANCE STATUS RECORD                   
         LA    R0,IO                                                            
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   DMPSW,C' '          CLEAR DUMP  SWITCH                           
         MVI   WRTSW,C' '          CLEAR WRITE SWITCH                           
         MVI   KEYSW,C' '          CLEAR PRINT SWITCH                           
         AP    INCNT,=P'1'                                                      
*                                                                               
*******              TEST FOR MATCHING CHECK NUMBERS                            
*                                                                               
         LA    R3,IO+33                                                         
         USING PPCLELEM,R3                                                      
         MVI   ELCODE,X'01'        CLEARANCE DETAILS ELEMENT CODE               
         CLC   ELCODE,PPCLEL01     ELEMENT FOUND ?                              
         BE    MAIN40              YES - GO TEST FOR CHECK CHANGES              
         DC    H'0'                MUST BE AT LEAST ONE ELEMENT                 
*                                                                               
MAIN30   BAS   RE,NEXTEL           LOOK FOR ANOTHER ELEMENT                     
         BE    MAIN90              NO MORE - GO TEST FOR OUTPUT                 
*                                                                               
MAIN40   DS    0H                                                               
         CLI   CLRDAT,0            CLEARANCE DATE ENTERED ?                     
         BE    MAIN40A             NO                                           
         CLC   PPCLCLRD,CLRDAT                     CLEARANCE DATE               
         BNE   MAIN30                              NEXT ELEMENT                 
MAIN40A  LA    R7,CKTAB                                                         
MAIN42   CLI   0(R7),X'FF'         END OF TABLE ?                               
         BE    MAIN30              YES - NEXT ELEMENT                           
         CLC   PPCLCHK(8),0(R7)    MATCH ON CHECK# AND DATE ?                   
         BE    MAIN44              YES - GO CORRECT CHECK DATA                  
         LA    R7,CKLNTH(R7)       NO - BUMP TO NEXT TABLE ENTRY                
         B     MAIN42                                                           
*                                                                               
MAIN44   DS    0H                                                               
         CLI   DMPSW,C'Y'          HAS RECORD BEEN DUMPED ?                     
         BE    MAIN46              YES                                          
*                                                                               
         CLI   QOPT2,C'Y'          DUMP RECORD ?                                
         BNE   MAIN46              NO                                           
         CP    OUTCNT,=P'25'       25 RECORDS DUMPED ?                          
         BH    MAIN46              YES - NO MORE DUMPING                        
         MVC   P(14),=C'*** BEFORE ***'                                         
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
MAIN46   DS    0H                                                               
         AP    CHGCNT,=P'1'        NUMBER OF ELEMENTS CHANGED                   
         MVI   WRTSW,C'Y'          RECORD HAS BEEN CHANGED                      
         MVC   P+55(6),PPCLCHK                                                  
         GOTO1 DATCON,DMCB,(2,PPCLCHDT),(10,P+62)                               
*                                                                               
         MVC   PPCLCHK(8),8(R7)    INSERT NEW CHECK# AND DATE                   
*                                                                               
         MVC   P+73(6),PPCLCHK                                                  
         GOTO1 DATCON,DMCB,(2,PPCLCHDT),(10,P+80)                               
         BAS   RE,PRNTMSG          PRINT CHANGE INFORMATION                     
         B     MAIN30              GO TEST NEXT ELEMENT                         
*                                                                               
MAIN90   DS    0H                  WRITE RECORD IF CHANGED                      
         CLI   WRTSW,C'Y'          HAS RECORD BEEN CHANGED ?                    
         BNE   MAIN00              NO - GET NEXT RECORD                         
         AP    OUTCNT,=P'1'                                                     
         CLI   QOPT2,C'Y'          DUMP RECORD ?                                
         BNE   MAIN92              NO                                           
         CP    OUTCNT,=P'25'       25 RECORDS DUMPED ?                          
         BH    MAIN92              YES - NO MORE DUMPING                        
         MVC   P(14),=C'*** AFTER  ***'                                         
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
MAIN92   DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   MAIN00              GET NEXT RECORD                              
         LA    RF,IO                                                            
         ST    RF,AREC                                                          
         GOTO1 PUTPRT                                                           
         B     MAIN00              GET NEXT RECORD                              
         DROP  R3                                                               
*                                                                               
         SPACE 2                                                                
*                                                                               
NEXTEL   NTR1                                                                   
NEXTELC  CLI   0(R3),0                                                          
         BE    NEXTELX             END OF RECORD - LEAVE CC EQUAL               
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   ELCODE,0(R3)        ELEMENT FOUND ?                              
         BNE   NEXTELC             NO - LOOK AT "NEXT"                          
         LTR   R3,R3               YES - SET CC TO NOT EQUAL                    
NEXTELX  XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DC    PL8'0'                                                           
         DC    CL20'STATUS RECS READ   '                                        
OUTCNT   DC    PL8'0'                                                           
         DC    CL20'STATUS RECS CHANGED'                                        
CHGCNT   DC    PL8'0'                                                           
         DC    CL20'ELEMENTS CHANGED   '                                        
         DC    X'FF'                                                            
*                                                                               
EOFSW    DC    C'N'                                                             
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL10   CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(20),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,28(R4)                                                        
         B     RUNL10                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
PRNTMSG  NTR1                     PRINT KEY FIELDS AND CHECK INFO.              
         CLI   KEYSW,C'Y'                                                       
         BE    PRNTOUT                                                          
         BAS   RE,DMPKEY                                                        
PRNTOUT  DS    0H                                                               
         BAS   RE,RPRT                                                          
         MVI   KEYSW,C'Y'                                                       
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVC   HEAD5+49(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+55(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R6,17               "SIGNIFICANT" KEY LENGTH                     
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R6),=C'N'                                 
*                                                                               
         MVC   WORK2(17),0(R5)                                                  
         TR    WORK2(17),TRTAB                                                  
         MVC   P+95(17),WORK2                                                   
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         MVI   DMPSW,C'Y'          RECORD HAS BEEN DUMPED                       
         L     R5,AREC                                                          
         MVC   HALF,25(R5)                                                      
         LH    R6,HALF              USE RECORD LENGTH                           
         LA    R3,0(R5,R6)                                                      
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
CKTAB    DS    0C                  DATES ARE COMPRESSED                         
*        ***** OLD CHK # & DATE  NEW CHK # & DATE *****                         
*****    DC    C'      ',X'0000',C'      ',X'0000'                              
         DC    X'000000000000',X'0000',C'829564',X'C0A9'                        
CKLNTH   EQU   *-CKTAB                                                          
*  INSERT ADDITIONAL CHECK CORRECTIONS AS ABOVE AFTER THIS *                    
*****    DC    C'222222',X'B75E',C'999999',X'B75E'                              
*****    DC    C'333333',X'0000',C'333333',X'B75E'                              
         DC    X'FF'               END OF TABLE INDICATOR                       
         EJECT                                                                  
*                                                                               
PPFSWRKD DSECT                                                                  
WORK2    DS    CL64                                                             
ELCODE   DS    X                                                                
ELEM     DS    CL200                                                            
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ITOT     DS    F                                                                
SAVERE   DS    F                                                                
ENDTST   DS    F                                                                
SVEST    DS    H                   BINARY ESTIMATE NUM                          
NEWNUM   DS    XL6                 PUB PRINTPAK KEY FORMAT                      
SVESTST  DS    CL6                 ESTIMATE START DATE - YYMMDD                 
SVESTEND DS    CL6                 ESTIMATE END   DATE - YYMMDD                 
SVLINE   DS    X                   LINE NUMBER FOR BUY KEY (BIN)                
SVREPO   DS    X                   REPO QUALITY FOR TSHT ELEM (BIN)             
SVIDAT   DS    XL3                 BINARY (YMD) INSERTION DATE                  
CLRDAT   DS    XL3                 BINARY (YMD) CHECK CLEARANCE DATE            
DMPSW    DS    X                   "Y" = RECORD HAS BEEN DUMPED                 
WRTSW    DS    X                   "Y" = RECORD IS TO BE WRITTEN                
KEYSW    DS    X                   "Y" = RECORD KEY HAS BEEN PRINTED            
KEY2     DS    CL25                                                             
IO       DS    1000X                                                            
REPS     DS    CL4                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPCLRST                                                        
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
*                                                                               
NORECD   DSECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015PPREPFS02 07/17/96'                                      
         END                                                                    
