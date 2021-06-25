*          DATA SET PPREP0202V AT LEVEL 019 AS OF 05/01/02                      
*PHASE PP0202V,+0,NOAUTO                                                        
*INCLUDE PPGETSPC                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL CHECK VALUES RETURNED FROM PPBVAL                    
*        OR PPGETSPC                                                            
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'                                                    
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
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
*                                                                               
RUNF     DS    0H                                                               
         ZAP   BUYCNT,=P'0'    BUY HEADER COUNT                                 
         ZAP   ELECNT,=P'0'    BILL ELEMENT COUNT                               
         LA    R0,PBUYREC                                                       
         ST    R0,ADBUY                                                         
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGENCY                              
         MVI   KEY+3,X'20'         BUYRECS                                      
*                                                                               
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         CLI   KEY+3,X'20'                                                      
         BE    AGYC6                                                            
         BH    NEXTAM                                                           
         MVI   KEY+3,X'20'                                                      
         B     AGYC2                                                            
*                                                                               
AGYC6    DS    0H                                                               
         GOTO1 GETBUY                                                           
         AP    BUYCNT,=P'1'                                                     
******************************************************                          
         B     AGYC10           SKIP TO CHECK PPGETSPC                          
******************************************************                          
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
AGYC8    BAS   RE,NEXTEL                                                        
         BNE   AGYC10                                                           
         USING PBILELEM,R2                                                      
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'E',PBILELEM),PPBVWORK,0                       
*                                                                               
         CLC   PBGROSS(12),PPBVEEG                                              
         BE    AGYC8                                                            
         AP    ELECNT,=P'1'                                                     
         BAS   RE,DMPKEY                                                        
         MVC   P2+1(5),=C'GROSS'                                                
         GOTO1 HEXOUT,DMCB,PBGROSS,P2+20,12,=C'N'                               
         MVC   P3+1(10),=C'EFF. GROSS'                                          
         GOTO1 HEXOUT,DMCB,PPBVEEG,P3+20,12,=C'N'                               
         BAS   RE,RPRT                                                          
         B     AGYC8                                                            
*                                                                               
AGYC10   DS    0H                                                               
         MVC   SVMCD,PBDMDATE                                                   
*                                                                               
         XC    PBDMDATE,PBDMDATE     CLEAR SO PPGETSPC WILL                     
*                                    CALCULATE ONE                              
         GOTO1 =V(PPGETSPC),DMCB,(C'B',PBUYREC),DATAMGR,ADDAY                   
         CLI   DMCB,X'FF'                                                       
         BNE   AGYC10E                                                          
         BAS   RE,DMPKEY                                                        
         MVC   P1(22),=C'*** PPGETSPC ERROR ***'                                
         BAS   RE,RPRT                                                          
         B     AGYC10X            GOT TO NEXT BUY                               
*                                                                               
AGYC10E  DS    0H                                                               
*                                                                               
         MVC   VALUES(8),DMCB     SAVE RETURNED VALUES                          
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   P1+0(27),=C'OLD MATERIALS CLOSING DATE='                         
         GOTO1 HEXOUT,DMCB,SVMCD,P1+30,3,=C'N'                                  
         MVC   P2+0(27),=C'NEW MATERIALS CLOSING DATE='                         
         GOTO1 HEXOUT,DMCB,VALUES,P2+30,3,=C'N'                                 
         MVC   P2+40(13),=C'OVERAGE PCT.='                                      
         GOTO1 HEXOUT,DMCB,VALUES+4,P2+55,1,=C'N'                               
         BAS   RE,RPRT                                                          
AGYC10X  DS    0H                                                               
         BAS   RE,RPRT                                                          
*                         MUST RESTORE FOR SEQUENTIAL READING                   
         GOTO1 HIGH                                                             
         B     AGYC3                                                            
*                                                                               
NEXTAM   DS    0H                                                               
         MVC   WORK(3),KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),WORK                                                      
         MVI   KEY+3,X'FF'                                                      
         B     AGYC2                                                            
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
BUYCNT   DS    PL8                                                              
         DC    CL15'BUYS READ'                                                  
ELECNT   DS    PL8                                                              
         DC    CL15'ELEMS FOUND'                                                
         DC    X'FF'                                                            
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P1(15),8(R4)                                                     
         OI    7(R4),X'0F'                                                      
         UNPK  P1+20(10),0(8,R4)                                                
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
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
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P1+01,(R2),=C'N'                                
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P1+75(25),WORK                                                   
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         LA    R2,220                                                           
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
         MVC   P1+01(8),WORK+00                                                 
         MVC   P1+10(8),WORK+08                                                 
         MVC   P1+19(8),WORK+16                                                 
         MVC   P1+28(8),WORK+24                                                 
         MVC   P1+37(8),WORK+32                                                 
         MVC   P1+46(8),WORK+40                                                 
         MVC   P1+55(8),WORK+48                                                 
         MVC   P1+64(8),WORK+56                                                 
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P1+75(0),WORK                                                    
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
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
SVMCD    DS    XL3                                                              
VALUES   DS    CL8            SAVED PPGETSPC VALUES                             
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
PPBVWORK DS    0D                                                               
       ++INCLUDE PPBVALD                                                        
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPREP0202V05/01/02'                                      
         END                                                                    
