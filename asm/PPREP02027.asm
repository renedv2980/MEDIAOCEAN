*          DATA SET PPREP02027 AT LEVEL 197 AS OF 05/01/02                      
*PHASE PP02027,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL READ PUBS AND CHANGE CHECK FOR UNCONVERTED           
*        SUPPLEMENTAL ADDRESS ELEMENTS                                          
*        X'1132' - LENGHT SHOULD BE X'113E'                                     
*        AND BAD X'40' ELEMENTS                                                 
*        AND BAD X'08',X'09',X'0A',X'0B' ELEMENTS - LENGTH S/B X'A5'            
*                                                                               
PP0202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0202                                                         
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
         ZAP   INCNT,=P'0'                                                      
         ZAP   CNT08,=P'0'                                                      
         ZAP   CNT09,=P'0'                                                      
         ZAP   CNT0A,=P'0'                                                      
         ZAP   CNT0B,=P'0'                                                      
         ZAP   CNT11,=P'0'                                                      
         ZAP   CNT40,=P'0'                                                      
         ZAP   BAD08,=P'0'                                                      
         ZAP   BAD09,=P'0'                                                      
         ZAP   BAD0A,=P'0'                                                      
         ZAP   BAD0B,=P'0'                                                      
         ZAP   BAD11,=P'0'                                                      
         ZAP   BAD40,=P'0'                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVC   PPGKEY,KEY         SAVE PPG'S KEY                                
         XC    KEY,KEY            TO BE SURE I MUST REREAD RECORD               
         MVC   KEY(25),PUBREC                                                   
         GOTO1 HIGHPUB                                                          
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 MUST FIND                                   
         GOTO1 GETNAME                                                          
*                                                                               
         AP    INCNT,=P'1'                                                      
*                                                                               
PROC08   DS    0H                                                               
         LA    R2,PUBREC+33       LOOK FOR AOR PUB LINK ELEM                    
         MVI   ELCODE,X'08'                                                     
PROC08C  BAS   RE,NEXTEL                                                        
         BNE   PROC09                                                           
         AP    CNT08,=P'1'                                                      
*                                                                               
         CLI   1(R2),X'A5'                                                      
         BE    PROC08C                                                          
*                                                                               
         MVC   P+10(12),=C'BAD X08 ELEM'                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   P+2(4),=C'KEY='                                                  
         GOTO1 HEXOUT,DMCB,KEY,P+8,25,=C'N'                                     
         MVC   P+110(10),=C'DISK ADDR='                                         
         GOTO1 HEXOUT,DMCB,KEY+27,P+122,4,=C'N'                                 
         BAS   RE,RPRT                                                          
         AP    BAD08,=P'1'                                                      
         B     PROC08C                                                          
*                                                                               
PROC09   DS    0H                                                               
         LA    R2,PUBREC+33       LOOK FOR AOR PUB LINK ELEM                    
         MVI   ELCODE,X'09'                                                     
PROC09C  BAS   RE,NEXTEL                                                        
         BNE   PROC0A                                                           
         AP    CNT09,=P'1'                                                      
*                                                                               
         CLI   1(R2),X'A5'                                                      
         BE    PROC09C                                                          
*                                                                               
         MVC   P+10(12),=C'BAD X09 ELEM'                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   P+2(4),=C'KEY='                                                  
         GOTO1 HEXOUT,DMCB,KEY,P+8,25,=C'N'                                     
         MVC   P+110(10),=C'DISK ADDR='                                         
         GOTO1 HEXOUT,DMCB,KEY+27,P+122,4,=C'N'                                 
         BAS   RE,RPRT                                                          
         AP    BAD09,=P'1'                                                      
         B     PROC09C                                                          
*                                                                               
PROC0A   DS    0H                                                               
         LA    R2,PUBREC+33       LOOK FOR AOR PUB LINK ELEM                    
         MVI   ELCODE,X'0A'                                                     
PROC0AC  BAS   RE,NEXTEL                                                        
         BNE   PROC0B                                                           
         AP    CNT0A,=P'1'                                                      
*                                                                               
         CLI   1(R2),X'A5'                                                      
         BE    PROC0AC                                                          
*                                                                               
         MVC   P+10(12),=C'BAD X0A ELEM'                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   P+2(4),=C'KEY='                                                  
         GOTO1 HEXOUT,DMCB,KEY,P+8,25,=C'N'                                     
         MVC   P+110(10),=C'DISK ADDR='                                         
         GOTO1 HEXOUT,DMCB,KEY+27,P+122,4,=C'N'                                 
         BAS   RE,RPRT                                                          
         AP    BAD0A,=P'1'                                                      
         B     PROC0AC                                                          
*                                                                               
PROC0B   DS    0H                                                               
         LA    R2,PUBREC+33       LOOK FOR AOR PUB LINK ELEM                    
         MVI   ELCODE,X'0B'                                                     
PROC0BC  BAS   RE,NEXTEL                                                        
         BNE   PROC11                                                           
         AP    CNT0B,=P'1'                                                      
*                                                                               
         CLI   1(R2),X'A5'                                                      
         BE    PROC0BC                                                          
*                                                                               
         MVC   P+10(12),=C'BAD X0B ELEM'                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   P+2(4),=C'KEY='                                                  
         GOTO1 HEXOUT,DMCB,KEY,P+8,25,=C'N'                                     
         MVC   P+110(10),=C'DISK ADDR='                                         
         GOTO1 HEXOUT,DMCB,KEY+27,P+122,4,=C'N'                                 
         BAS   RE,RPRT                                                          
         AP    BAD0B,=P'1'                                                      
         B     PROC0BC                                                          
*                                                                               
PROC11   DS    0H                                                               
         LA    R2,PUBREC+33       LOOK FOR AOR PUB LINK ELEM                    
         MVI   ELCODE,X'11'                                                     
PROC11C  BAS   RE,NEXTEL                                                        
         BNE   PROC40                                                           
         AP    CNT11,=P'1'                                                      
*                                                                               
         CLI   1(R2),X'3E'                                                      
         BE    PROC11C                                                          
*                                                                               
         MVC   P+10(12),=C'BAD X11 ELEM'                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   P+2(4),=C'KEY='                                                  
         GOTO1 HEXOUT,DMCB,KEY,P+8,25,=C'N'                                     
         MVC   P+110(10),=C'DISK ADDR='                                         
         GOTO1 HEXOUT,DMCB,KEY+27,P+122,4,=C'N'                                 
         BAS   RE,RPRT                                                          
         AP    BAD11,=P'1'                                                      
         B     PROC11C                                                          
*                                                                               
PROC40   DS    0H                                                               
         LA    R2,PUBREC+33       LOOK FOR AOR PUB LINK ELEM                    
         MVI   ELCODE,X'40'                                                     
PROC40C  BAS   RE,NEXTEL                                                        
         BNE   PROCX               GO GET NEXT RECORD                           
         AP    CNT40,=P'1'                                                      
*                                                                               
         CLI   1(R2),X'0A'                                                      
         BE    PROC40C                                                          
*                                                                               
         MVC   P+10(12),=C'BAD X40 ELEM'                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   P+2(4),=C'KEY='                                                  
         GOTO1 HEXOUT,DMCB,KEY,P+8,25,=C'N'                                     
         MVC   P+110(10),=C'DISK ADDR='                                         
         GOTO1 HEXOUT,DMCB,KEY+27,P+122,4,=C'N'                                 
         BAS   RE,RPRT                                                          
         AP    BAD40,=P'1'                                                      
         B     PROC40C                                                          
*                                                                               
******************************** OLD CODE                                       
         USING PUBADVEL,R2                                                      
         CLC   PUBADVCD,QPRODUCT                                                
         BNE   PROC5                                                            
         CLC   PUBAORCD(2),QOPT1                                                
         BNE   PROC5                                                            
         AP    CLTCNT,=P'1'                                                     
*                                                                               
         CLI   QOPT7,C'D'                                                       
         BNE   PROC9                                                            
*                                                                               
         GOTO1 RECUP,DMCB,(1,PUBREC),0(R2),0                                    
         AP    DELCNT,=P'1'                                                     
         B     PROC22                                                           
*                                                                               
PROC9    XC    ELEM,ELEM                                                        
         ZIC   R1,1(R2)               ELEM LENGTH                               
         BCTR  R1,0                                                             
         EX    R1,MOVE                                                          
         LA    R2,ELEM                                                          
         MVC   PUBAORCD(2),QOPT3      CHANGE TO NEW                             
*                                                                               
         DROP  R2                                                               
*                                                                               
PROC10   DS    0H                                                               
*                                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'FF'                                                     
PROC20   BAS   RE,NEXTEL                                                        
         BE    PROC20                                                           
*                                                                               
         GOTO1 RECUP,DMCB,(1,PUBREC),ELEM,0(R2)                                 
         AP    CNT08,=P'1'                                                      
*                                                                               
PROC22   LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
*                                                                               
CLEARREC SR    R5,R5             CLEAR END OF RECORD                            
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
         CLI   RCWRITE,C'N'                                                     
         BE    PROC25                                                           
         GOTO1 PUTPUB                                                           
*                                                                               
PROC25   MVC   P+2(4),=C'KEY='                                                  
         GOTO1 HEXOUT,DMCB,KEY,P+8,25,=C'N'                                     
         MVC   P+110(10),=C'DISK ADDR='                                         
         GOTO1 HEXOUT,DMCB,KEY+27,P+122,4,=C'N'                                 
         BAS   RE,RPRT                                                          
*                                                                               
         CP    CLTCNT,=P'25'                                                    
         BH    PROCX                                                            
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         MVI   FORCEHED,C'Y'   SKIP AFTER                                       
         B     PROCX                                                            
*                                                                               
PROCX    MVC   KEY(64),PPGKEY     RESTORE KEY AND KEYSAVE                       
         GOTO1 HIGHPUB            RESTORE PPG'S SEQ READ                        
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
PROC50   DS    0H                                                               
         CLI   QOPT7,C'Y'            SEE IF CREATING NEW ELEMS                  
         BNE   PROCX                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING PUBADVEL,R2                                                      
         MVC   0(2,R2),=X'800F'                                                 
         MVC   PUBADVCD,QPRODUCT                                                
         MVC   PUBAORCD(2),QOPT3                                                
         MVC   PUBADVPC,PUBKPUB     USE THIS PUB CODE                           
         B     PROC10                REST SAME AS PROC10                        
*                                                                               
         DROP  R2                                                               
*                                                                               
MOVE     MVC   ELEM(0),0(R2)         EXECUTED                                   
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
         MVI   RCSUBPRG,10                                                      
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         GOTO1 SEQPUB                                                           
         CLC   KEY(9),PUBKEY                                                    
         BNE   NPX                                                              
         GOTO1 GETLTL                                                           
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
         SPACE 3                                                                
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
CNT08    DS    PL8                                                              
         DC    CL15'08 FOUND'                                                   
CNT09    DS    PL8                                                              
         DC    CL15'09 FOUND'                                                   
CNT0A    DS    PL8                                                              
         DC    CL15'0A FOUND'                                                   
CNT0B    DS    PL8                                                              
         DC    CL15'0B FOUND'                                                   
CNT11    DS    PL8                                                              
         DC    CL15'11 FOUND'                                                   
CNT40    DS    PL8                                                              
         DC    CL15'40 FOUND'                                                   
BAD08    DS    PL8                                                              
         DC    CL15'BAD 08 ELEMS'                                               
BAD09    DS    PL8                                                              
         DC    CL15'BAD 09 ELEMS'                                               
BAD0A    DS    PL8                                                              
         DC    CL15'BAD 0A ELEMS'                                               
BAD0B    DS    PL8                                                              
         DC    CL15'BAD 0B ELEMS'                                               
BAD11    DS    PL8                                                              
         DC    CL15'BAD 11 ELEMS'                                               
BAD40    DS    PL8                                                              
         DC    CL15'BAD 40 ELEMS'                                               
         DC    X'FF'                                                            
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
*                                                                               
         BUFF  LINES=2000,ROWS=1,COLUMNS=6,FLAVOR=PACKED,KEYLIST=(10,A)X        
               ,COMMENT=10                                                      
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
PUBADVED DSECT                                                                  
       ++INCLUDE PPPUBADVEL                                                     
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'197PPREP0202705/01/02'                                      
         END                                                                    
