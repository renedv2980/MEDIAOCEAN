*          DATA SET PPREP0302Z AT LEVEL 028 AS OF 05/01/02                      
*PHASE PP0302Z,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE PPGETSIZ                                                               
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL READ PUBS TO TEST PPGETSIZ RM                        
*                                                                               
*        QOPT1       USE FOR BLEED (B)                                          
*        QOPT2,3,4   USE FOR SIZE CODE                                          
*        QOPT6       USE FOR SRDS OK (1)                                        
*                                                                               
         PRINT NOGEN                                                            
PP0302   CSECT                                                                  
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
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         B     EXIT                                                             
*                                                                               
REQF     DS    0H                  FIRST PUB FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'          RESET PAGE NUM                               
****     CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
****     BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
PROC     DS    0H                                                               
         CLC   QAGENCY,PUBKAGY     SAME AGENCY ?                                
         BNE   EXIT                NO - IGNORE                                  
         AP    INCNT,=P'1'                                                      
*                                                                               
         MVI   BYTE,0                                                           
         CLI   QOPT6,C'1'          OK TO CHECK SRDS FILE                        
         BNE   *+8                                                              
         MVI   BYTE,1                                                           
*                                                                               
         GOTO1 =V(PPGETSIZ),DMCB,(BYTE,PUBREC),(QOPT1,QOPT2),DATAMGR,  X        
               UTL,0                                                            
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   TEST5               NO                                           
         MVC   P+10(13),=C'ERROR IN CALL'                                       
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
*                                                                               
TEST5    MVC   WORK(1),0(R1)       P, S, OR NULL                                
         MVI   0(R1),0                                                          
         L     R5,0(R1)                                                         
         MVC   ELEM(L'ELEM),0(R5)  200 BYTES FOR TEST                           
*                                                                               
*                                                                               
*                            ***** PRINT **************                         
         CLI   WORK,0                                                           
         BNE   TEST20                                                           
         B     EXIT                                                             
TEST20   MVC   P+1(2),PUBKAGY                                                   
         GOTO1 PUBEDIT,DMCB,PUBKPUB,P+4                                         
         MVC   P+26(1),WORK                                                     
         LA    R5,ELEM                                                          
         MVC   P+28(4),0(R5)                                                    
         BAS   RE,RPRT                                                          
         MVC   P+15(9),=C'DIMENSION'                                            
         GOTO1 HEXOUT,DMCB,04(R5),P+28,09,0                                     
         BAS   RE,RPRT                                                          
         MVC   P+15(4),=C'TRIM'                                                 
         GOTO1 HEXOUT,DMCB,13(R5),P+28,09,0                                     
         BAS   RE,RPRT                                                          
         MVC   P+15(8),=C'SAFETY 1'                                             
         GOTO1 HEXOUT,DMCB,22(R5),P+28,06,0                                     
         BAS   RE,RPRT                                                          
         MVC   P+15(8),=C'SAFETY 2'                                             
         GOTO1 HEXOUT,DMCB,28(R5),P+28,06,0                                     
         BAS   RE,RPRT                                                          
         MVC   P+15(8),=C'SAFETY 3'                                             
         GOTO1 HEXOUT,DMCB,34(R5),P+28,06,0                                     
         BAS   RE,RPRT                                                          
         MVC   P+15(8),=C'SAFETY 4'                                             
         GOTO1 HEXOUT,DMCB,40(R5),P+28,06,0                                     
         BAS   RE,RPRT                                                          
         MVC   P+15(8),=C'SAFETY 5'                                             
         GOTO1 HEXOUT,DMCB,46(R5),P+28,06,0                                     
         BAS   RE,RPRT                                                          
         MVC   P+15(8),=C'SAFETY 6'                                             
         GOTO1 HEXOUT,DMCB,52(R5),P+28,06,0                                     
         BAS   RE,RPRT                                                          
         BAS   RE,RPRT                                                          
*                                                                               
TESTXIT  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
PROCX    DS    0H                                                               
         MVC   KEY(64),SKEY        RESTORE PPG'S KEY                            
         B     EXIT                                                             
*                                                                               
*                             ***  NOT USED IN THIS PROGRAM  ***                
NEXTCLT  DS    0H                                                               
         ZIC   R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(6),PUBKPUB                                                 
         MVI   KEY+3,X'21'                                                      
         XC    KEY+13(12),KEY+13                                                
*****    B     PROC20                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         XC    P,P                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNLX                                                            
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
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
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1          ***  NOT USED IN THIS PROGRAM  ***                       
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
         MVC   HALF,25(R5)      RECORD LENGTH                                   
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   DMPRECX                                                          
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
*                                                                               
DMPRECX  BAS   RE,RPRT      SKIP BETWEEN RECORDS                                
         B     EXIT                                                             
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
MATCNT   DS    PL8                                                              
         DC    CL15'SAME LEVEL ADDR'                                            
OFCCNT   DS    PL8                                                              
         DC    CL15'AT OFFICE LEVEL'                                            
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
WRKREP   DS    CL4                                                              
SVOFC    DS    CL3                                                              
LASTPUB  DS    XL6                                                              
LASTREP  DS    XL8                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
ELEM     DS    CL200                                                            
*                                                                               
BUFREC   DS    0CL68                                                            
BUFKEY   DS    0CL10                                                            
BUFTYPE  DS    CL1                                                              
BUFCLT   DS    CL3                                                              
BUFPUB   DS    CL6                                                              
BUFCOM   DS    CL10                                                             
*                                                                               
BUFNET   DS    PL8                                                              
BUFCD    DS    PL8                                                              
BUFTAX   DS    PL8                                                              
BUFBASIS DS    PL8                                                              
BUFGST   DS    PL8                                                              
BUFGSTPD DS    PL8                                                              
*                                                                               
*                                                                               
       ++INCLUDE GVALUES                                                        
*                                                                               
       ++INCLUDE PBILPROF                                                       
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028PPREP0302Z05/01/02'                                      
         END                                                                    
