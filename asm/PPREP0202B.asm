*          DATA SET PPREP0202B AT LEVEL 143 AS OF 05/01/02                      
*PHASE PP0202B,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE GETINS                         3/5/97 - WAS GETINS                     
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM UNDELETES BUYS                                            
*                                                                               
*        QPAY   CAN BE USED TO CHECK FOR                                        
*               ONE DELETION DATE                                               
*                                                                               
         PRINT  NOGEN                                                           
PP0202   CSECT                                                                  
         NMOD1 0,PP0202,R7         *** NOTE USE OF R7 AS 2ND BASE               
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
         CLI   MODE,FBUYREQ                                                     
         BE    FIRSTBUY                                                         
         CLI   MODE,LBUYREQ                                                     
         BE    LASTBUY                                                          
         CLI   MODE,PROCBUY                                                     
         BE    PRBUY                                                            
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
PRBUY    DS    0H                                                               
         TM    PBUYCNTL,X'80'        SEE IF DELETED                             
         BNO   EXIT                                                             
         CLI   DELDATE,0                                                        
         BE    PRB1                                                             
         CLC   PBDDATE,DELDATE       MUST MATCH DATES                           
         BNE   EXIT                                                             
*                                                                               
PRB1     DS    0H                                                               
         MVC   P+1(6),=C'BEFORE'                                                
         BAS   RE,RPRT                                                          
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,DMPREC                                                        
         MVI   PBUYCNTL,X'00'        UN-DELETE                                  
*                                                                               
         MVC   P+1(5),=C'AFTER'                                                 
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRB2                                                             
         GOTO1 PUTPRT                RE-WRITE BUY                               
PRB2     AP    BUYS,=P'1'                                                       
         MVC   MYKEY,KEY                                                        
         GOTO1 READ                                                             
         CLC   KEY(25),MYKEY                                                    
         BE    *+6                                                              
         DC    H'0'    MUST FIND                                                
         MVC   P+1(6),=C'BEFORE'                                                
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         MVI   KEY+25,X'00'          UNDELETE POINTER                           
         MVC   P+1(5),=C'AFTER'                                                 
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRB4                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,PRTDIR,KEY,KEY                                
PRB4     AP    PT20,=P'1'                                                       
         MVC   SAVKEY,KEY                                                       
         MVI   KEY+3,X'21'      NOW DO 21 POINTER                               
         XC    KEY+7(9),KEY+7                                                   
         MVC   KEY+7(6),SAVKEY+10      PUB                                      
         MVC   KEY+13(3),SAVKEY+7      PRD                                      
         MVC   MYKEY,KEY                                                        
         GOTO1 READ                                                             
         CLC   KEY(25),MYKEY                                                    
         BE    *+6                                                              
         DC    H'0'    MUST FIND                                                
         MVC   P+1(6),=C'BEFORE'                                                
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         MVI   KEY+25,X'00'      UNDELETE X'21' POINTER                         
         MVC   P+1(5),=C'AFTER'                                                 
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   PRB6                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,PRTDIR,KEY,KEY                                
PRB6     AP    PT21,=P'1'                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),SAVKEY                                                   
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
FIRSTBUY DS    0H                                                               
         OI    DMINBTS,X'08'      PASS DELETES                                  
         CLI   QOPT1,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         ZAP   BUYS,=P'0'                                                       
         ZAP   PT20,=P'0'                                                       
         ZAP   PT21,=P'0'                                                       
*                                                                               
         XC    DELDATE,DELDATE                                                  
         CLI   QPAY,C' '         SEE IF I HAVE A DATE                           
         BNH   EXIT                                                             
         GOTO1 DATCON,DMCB,(0,QPAY),(3,DELDATE)                                 
         B     EXIT                                                             
*                                                                               
TAPEGET  NTR1                                                                   
         GET   IN,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0           END OF RECORD                                  
         XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
BUYS     DS    PL8                                                              
         DC    CL15'BUYS'                                                       
PT20     DS    PL8                                                              
         DC    CL15'20 POINTERS'                                                
PT21     DS    PL8                                                              
         DC    CL15'21 POINTERS'                                                
         DC    X'FF'                                                            
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
LASTBUY  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
LASTB5   CLI   0(R4),X'FF'                                                      
         BE    LASTB10                                                          
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     LASTB5                                                           
*                                                                               
LASTB10  DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
EOF      CLOSE (IN,)                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
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
*                                                                               
BUYADD   L     R0,0(R1)                                                         
         CVD   R0,DUB                                                           
         AP    0(8,R2),DUB                                                      
         LA    R1,4(R1)                                                         
         LA    R2,8(R2)                                                         
         BCT   R3,BUYADD                                                        
         BR    RE                 RETURN                                        
*                                                                               
ROLL     DS    0H                                                               
         LA    R3,18                                                            
ROLL5    AP    0(8,R2),0(8,R1)                                                  
         LA    R2,8(R2)                                                         
         LA    R1,8(R1)                                                         
         BCT   R3,ROLL5                                                         
         BR    RE                RETURN                                         
*                                                                               
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,27                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         LA    R2,280                                                           
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
TITLES   DS    0C                                                               
         DC    CL15'BUYS UNDELETED'                                             
         DC    CL15'20 UNDELETED'                                               
         DC    CL15'21 UNDELETED'                                               
         DC    CL15' '                                                          
*                                                                               
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
DELDATE  DS    XL3                                                              
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SAVKEY   DS    CL32                                                             
MYKEY    DS    CL25                                                             
*                                                                               
*          DATA SET PVALUES    AT LEVEL 005 AS OF 06/30/86                      
*                        *** OUTPUT PARAMETER BLOCK FOR GETINS ****             
NPVALUES DS    0F                                                               
*                                                                               
* ORDERED DATA                                                                  
*                                                                               
NGROSS    DS    F                   GROSS ORDERED                               
NAGYCOM   DS    F                   AGENCY COMMISSION                           
NCSHDSC   DS    F                   CASH DISCOUNT                               
NPYABLE   DS    F                   GROSS-AGYCOMM-CASHDSC                       
NBLABLE   DS    F                   GROSS-CASH DSC                              
NPREMIUM  DS    F                   (INCLUDED IN ABOVE FIELDS)                  
NUNITS    DS    F                   NUMBER OF LINES BOUGHT                      
*                                                                               
***** NOTE ORDERED TAX UNDER PAID DATA                                          
* PAID DATA                                                                     
*                                                                               
NPGROSS   DS    F                   GROSS PAID                                  
NPAGYCOM  DS    F                   AGY COMM PAID                               
NPCSHDSC  DS    F                   CASH DISCOUNT PAID                          
NPAID     DS    F                   ACTUAL PAID AMOUNT                          
*                                                                               
NTAX      DS    F                   ORDERED TAX - WAS PAYABLE DATE              
*                          (INCLUDED IN ORDERED GROSS,PYABLE,BLABLE)            
*                                  NET X PBDTAX (4 DECIMALS)                    
*                                                                               
* BILLED DATA                                                                   
*                                                                               
NBGROSS   DS    F                   GROSS BILLED                                
NBAGYCOM  DS    F                   AGY COMM BILLED                             
NBCSCHDS  DS    F                   CASH DISCOUNT BILLED                        
NBILLED   DS    F                   ACTUAL BILLED AMOUNT                        
NBLBLDT   DS    CL3                 BILLABLE DATE -YMD                          
*                                                                               
NPVALUEX DS    0C                                                               
*                                                                               
*          DATA SET GVALUES    AT LEVEL 003 AS OF 01/14/91                      
GVALUES  DS    0F                                                               
**************** GETINS OPTIONAL DATA                                           
GSPAYCNT DS    F                   COUNT OF PAID ELEMENTS                       
GSBILCNT DS    F                   COUNT OF BILLED ELEMETS                      
GSTTAX   DS    F                   PAYABYLE GST TAX                             
GSTTAXPD DS    F                   PAID GST TAX                                 
GSTCODE  DS    H                   GST CODE                                     
GSTPCT   DS    H                   GST %                                        
GSTBASIS DS    CL1                 X'-01' SALES TAX ON NET                      
         DS    CL3                 SPARE                                        
GSTTAXBL DS    F                   TAX BILLED                                   
********                                                                        
NVALUES  DS    0F                                                               
*************NEW GETINS OPTIONAL DATA                                           
NSPAYCNT DS    F                   COUNT OF PAID ELEMENTS                       
NSBILCNT DS    F                   COUNT OF BILLED ELEMETS                      
NSTTAX   DS    F                   PAYABYLE GST TAX                             
NSTTAXPD DS    F                   PAID GST TAX                                 
NSTCODE  DS    H                   GST CODE                                     
NSTPCT   DS    H                   GST %                                        
NSTBASIS DS    CL1                 X'-01' SALES TAX ON NET                      
         DS    CL3                 SPARE                                        
NSTTAXBL DS    F                   TAX BILLED                                   
*************                                                                   
GPVAL    DS    0F                                                               
**************** GETINS PST DATA                                                
GPVAL1   DS    CL180                                                            
GPVAL2   DS    CL180                                                            
*************                                                                   
NPVAL    DS    0F                                                               
**************** NEW GETINS PST DATA                                            
NPVAL1   DS    CL180                                                            
NPVAL2   DS    CL180                                                            
*************                                                                   
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'143PPREP0202B05/01/02'                                      
         END                                                                    
