*          DATA SET PPREP0202E AT LEVEL 014 AS OF 05/01/02                      
*PHASE PP0202C                                                                  
*INCLUDE MININAM                                                                
*                                                                               
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
* THIS PROGRAM COUNTS INVC RECORDS BY LENGTH IN INCREMENTS OF 500               
*                                                                               
PP0202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0202                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INV500,=P'0'                                                     
         ZAP   INV1000,=P'0'                                                    
         ZAP   INV1500,=P'0'                                                    
         ZAP   INV2000,=P'0'                                                    
         ZAP   INV2500,=P'0'                                                    
         ZAP   INV2900,=P'0'                                                    
         ZAP   INV3000,=P'0'                                                    
         ZAP   INV4000,=P'0'                                                    
         ZAP   INVCNT,=P'0'                                                     
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT1,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,PBUYREC          READ INTO BUY                                
         ST    R0,AREC                                                          
         XC    ELCNT,ELCNT         MAXIMUM # OF ELEMENTS IN RECORD              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'50'          INVOICES                                    
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
PROC3    GOTO1 SEQ                                                              
PROC5    CLC   KEY(4),KEYSAVE      JUST CHECK AGY/MEDIA RECORD CODE             
         BNE   PROC80              END OF MEDIA GO DO NEXT                      
         AP    INVCNT,=P'1'                                                     
******                                                                          
         GOTO1 GETPRT              GET THE INV REC                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R5,R5                                                            
         L     R4,AREC             POINT TO RECORD                              
         LA    R4,33(R4)                                                        
PROC5C   CLI   0(R4),0             END OF RECORD ?                              
         BE    PROC5X              YES                                          
         LA    R5,1(R5)            ADD TO ELEMENT COUNT                         
         ZIC   R0,1(R4)                                                         
         AR    R4,R0               NEXT ELEMENT                                 
         B     PROC5C                                                           
*                                                                               
PROC5X   DS    0H                                                               
         CH    R5,ELCNT                                                         
         BNH   PROC6               COUNT NOT "HIGHEST"                          
         STH   R5,ELCNT            NEW "HIGHEST" ELEMENT COUNT                  
*                                                                               
PROC6    DS    0H                      COUNT BY RECORD LENGTH                   
         ZICM  R5,PBUYLEN,2                                                     
         CHI   R5,499                                                           
         BH    PROC6A                                                           
         AP    INV500,=P'1'                                                     
         B     PROC3               GET NEXT INV                                 
PROC6A   CHI   R5,999                                                           
         BH    PROC6B                                                           
         AP    INV1000,=P'1'                                                    
         B     PROC3               GET NEXT INV                                 
PROC6B   CHI   R5,1499                                                          
         BH    PROC6C                                                           
         AP    INV1500,=P'1'                                                    
         B     PROC3               GET NEXT INV                                 
PROC6C   CHI   R5,1999                                                          
         BH    PROC6D                                                           
         AP    INV2000,=P'1'                                                    
         B     PROC3               GET NEXT INV                                 
PROC6D   CHI   R5,2499                                                          
         BH    PROC6E                                                           
         AP    INV2500,=P'1'                                                    
         B     PROC3               GET NEXT INV                                 
PROC6E   CHI   R5,2899                                                          
         BH    PROC6F                                                           
         AP    INV2900,=P'1'                                                    
         B     PROC3               GET NEXT INV                                 
PROC6F   CHI   R5,2999                                                          
         BH    PROC6G                                                           
         AP    INV3000,=P'1'                                                    
         B     PROC3               GET NEXT INV                                 
PROC6G   AP    INV4000,=P'1'       MUST BE GREATER THAN 3000                    
         B     PROC3               GET NEXT INV                                 
*                                                                               
*                                                                               
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'          END OF FILE                                   
         BE    EXIT                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'             SKIP TO NEXT AGY/MED                     
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'               END OF FILE                              
         BE    EXIT                                                             
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'50'         INVOICES                                     
         B     PROC2                                                            
*                                                                               
************************************* END OF NEW CODE                           
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
INVCNT   DS    PL8                                                              
         DC    CL15'INVS READ'                                                  
INV500   DS    PL8                                                              
         DC    CL15'INVS UNDER 500 '                                            
INV1000  DS    PL8                                                              
         DC    CL15'INVS UNDER 1000'                                            
INV1500  DS    PL8                                                              
         DC    CL15'INVS UNDER 1500'                                            
INV2000  DS    PL8                                                              
         DC    CL15'INVS UNDER 2000'                                            
INV2500  DS    PL8                                                              
         DC    CL15'INVS UNDER 2500'                                            
INV2900  DS    PL8                                                              
         DC    CL15'INVS UNDER 2900'                                            
INV3000  DS    PL8                                                              
         DC    CL15'INVS UNDER 3000'                                            
INV4000  DS    PL8                                                              
         DC    CL15'INVS OVER  3000'                                            
         DC    X'FF'                                                            
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+10(18),=C'MAX ELEMENT COUNT:'                                  
         EDIT  ELCNT,(3,P+29),ALIGN=LEFT                                        
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
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
         MVC   HEAD7+56(22),=C'INVS WITH MISSING PUBS'                          
         MVC   HEAD8+56(22),=C'----------------------'                          
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
         MVC   HALF,25(R5)                                                      
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
LASTPUB  DS    XL6                                                              
LASTCLT  DS    CL3                                                              
ELCODE   DS    X                                                                
MYKEY    DS    CL32          SAVE KEY                                           
MYKEYS   DS    CL32          SAVE KEYSAVE                                       
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
ELCNT    DS    H                                                                
*                                                                               
ESTTAB   DS    1000C                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014PPREP0202E05/01/02'                                      
         END                                                                    
