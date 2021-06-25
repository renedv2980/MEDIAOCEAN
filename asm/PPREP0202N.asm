*          DATA SET PPREP0202N AT LEVEL 129 AS OF 05/01/02                      
*PHASE PP0202N,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE GETINSH                        3/5/97 - WAS GETINS                     
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
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
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
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
         GOTO1 =V(GETINS),DMCB,PBUYREC,NGROSS,KEY+7,0,=C'BOTH'                  
         L     R5,DMCB+16                                                       
         MVC   NVALUES(28),0(R5)                                                
         MVC   NPVAL1(180),28(R5)                                               
         MVC   NPVAL2(180),208(R5)                                              
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,KEY+7,0,=C'BOTH'                       
         L     R5,DMCB+16                                                       
         MVC   GVALUES(28),0(R5)                                                
         MVC   GPVAL1(180),28(R5)                                               
         MVC   GPVAL2(180),208(R5)                                              
*                                                                               
         CLC   NVALUES(28),GVALUES                                              
         BNE   PRBUY5                                                           
         CLC   NPVAL1(180),GPVAL1                                               
         BNE   PRBUY5                                                           
         CLC   NGROSS(64),GROSS         CHECK FOR DIFFERENCE                    
         BE    EXIT                                                             
PRBUY5   BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   P+1(10),=C'NEW GETINS'                                           
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NGROSS,P+10,28,=C'N'                                 
         MVC   P+1(7),=C'ORDERED'                                               
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NPGROSS,P+10,20,=C'N'                                
         MVC   P+1(4),=C'PAID'                                                  
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NBGROSS,P+10,16,=C'N'                                
         MVC   P+1(6),=C'BILLED'                                                
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NVALUES,P+10,28,=C'N'                                
         MVC   P+1(3),=C'GST'                                                   
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NPVAL1,P+10,60,=C'N'                                 
         MVC   P+1(3),=C'PST'                                                   
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NPVAL1+60,P+10,60,=C'N'                              
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NPVAL1+120,P+10,60,=C'N'                             
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NPVAL1+180,P+10,60,=C'N'                             
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NPVAL1+240,P+10,60,=C'N'                             
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,NPVAL1+300,P+10,60,=C'N'                             
         BAS   RE,RPRT                                                          
***                                                                             
         MVC   P+1(10),=C'OLD GETINS'                                           
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GROSS,P+10,28,=C'N'                                  
         MVC   P+1(7),=C'ORDERED'                                               
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,PGROSS,P+10,20,=C'N'                                 
         MVC   P+1(4),=C'PAID'                                                  
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,BGROSS,P+10,16,=C'N'                                 
         MVC   P+1(6),=C'BILLED'                                                
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GVALUES,P+10,28,=C'N'                                
         MVC   P+1(3),=C'GST'                                                   
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GPVAL1,P+10,60,=C'N'                                 
         MVC   P+1(3),=C'PST'                                                   
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GPVAL1+60,P+10,60,=C'N'                              
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GPVAL1+120,P+10,60,=C'N'                             
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GPVAL1+180,P+10,60,=C'N'                             
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GPVAL1+240,P+10,60,=C'N'                             
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GPVAL1+300,P+10,60,=C'N'                             
         BAS   RE,RPRT                                                          
         B     EXIT                                                             
*                                                                               
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT1,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     EXIT                                                             
****************************OLD CODE                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         MVC   KEY+7(3),QPRODUCT                                                
         GOTO1 HIGH                                                             
         B     PROC5                                                            
PROC3    GOTO1 SEQ                                                              
PROC5    CLC   KEY(7),KEYSAVE                                                   
         BNE   PROCEND             DONE                                         
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   PBDCOSIN,C'S'                                                    
         BNE   PROC3                                                            
         LA    R2,PBDELEM                                                       
         MVI   ELCODE,X'99'             ONLY CHANGE CONVERTED BUYS              
         BAS   RE,NEXTEL                                                        
         BNE   PROC3                                                            
         B     PROC6                 DMPREC BYPASSED                            
*                                                                               
         LA    R1,NEWSCNT                                                       
         CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         LA    R1,MAGSCNT                                                       
         CP    0(8,R1),=P'10'      PDUMP FIRST 10 BUYS                          
         BH    PROC6                                                            
         BAS   RE,DMPREC                                                        
PROC6    GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         LA    R1,GROSS            ORDERED                                      
         LA    R2,ROG                                                           
         LA    R3,3                                                             
         BAS   RE,BUYADD                                                        
         LA    R1,PGROSS           PAID                                         
         LA    R2,RPG                                                           
         LA    R3,3                                                             
         BAS   RE,BUYADD                                                        
         LA    R1,BGROSS           BILLED                                       
         LA    R2,RBG                                                           
         LA    R3,3                                                             
         BAS   RE,BUYADD                                                        
*                                                                               
         B     PROC6D                SKIP HEXOUTS                               
*                                                                               
         GOTO1 HEXOUT,DMCB,GROSS,P+15,28,=C'N'                                  
         MVC   P+1(7),=C'ORDERED'                                               
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,PGROSS,P+15,20,=C'N'                                 
         MVC   P+1(4),=C'PAID'                                                  
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,BGROSS,P+15,16,=C'N'                                 
         MVC   P+1(6),=C'BILLED'                                                
         BAS   RE,RPRT                                                          
*                                                                               
PROC6D   MVI   PBDCOSIN,C' '                                                    
         ZAP   PBDACP,=P'15000'        RESET TO 15 PCT                          
         CP    PBDPRCOS,=P'0'            FIRST GROSS UP PREMIUM                 
         BE    PROC7                                                            
         ZAP   DUB,PBDPRCOS                                                     
         CVB   R1,DUB                                                           
         M     R0,=F'200'                                                       
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   PBDPRCOS,DUB                                                     
*                                                                               
PROC7    DS    0H                  BILL ELEMS                                   
         LA    R2,PBDELEM                                                       
         MVI   ELCODE,X'26'                                                     
PROC7B   BAS   RE,NEXTEL                                                        
         BNE   PROC9                                                            
         USING PBILELEM,R2                                                      
         OC    PBLDATE,PBLDATE                                                  
         BZ    PROC7B                                                           
*                                                                               
         L     R1,PBGROSS          NOW NET                                      
         M     R0,=F'200'                                                       
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         ST    R1,PBGROSS          STORE GROSS                                  
*                                                                               
PROC7D   ZAP   DUB,PBDACP                                                       
         CVB   R0,DUB                                                           
         L     RF,=F'100000'                                                    
         SR    RF,R0                                                            
         MR    RE,R1                                                            
         SLDL  RE,1                                                             
         D     RE,=F'100000'                                                    
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LR    R0,R1                                                            
         SR    R0,RF                                                            
         ST    R0,PBAGYCOM         SET BILLED AGYCOM                            
         B     PROC7B                                                           
         DROP  R2                                                               
*                                                                               
PROC9    DS    0H                  NOW DO PAY ELEMS                             
         LA    R2,PBDELEM                                                       
         MVI   ELCODE,X'25'                                                     
PROC9B   BAS   RE,NEXTEL                                                        
         BNE   PROC11                                                           
         USING PPAYELEM,R2                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    PROC9B                                                           
*                                                                               
         L     R1,PPGROSS          NOW NET                                      
         M     R0,=F'200'                                                       
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         ST    R1,PPGROSS          STORE GROSS                                  
*                                                                               
         ZAP   DUB,PBDACP                                                       
         CVB   R0,DUB                                                           
         L     RF,=F'100000'                                                    
         SR    RF,R0                                                            
         MR    RE,R1                                                            
         SLDL  RE,1                                                             
         D     RE,=F'100000'                                                    
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         LR    R0,R1                                                            
         SR    R0,RF                                                            
         ST    R0,PPAGYCOM         SET PAID AGYCOM                              
         B     PROC9B                                                           
         DROP  R2                                                               
*                                                                               
PROC11   DS    0H                  NOW GROSS-UP PBDCOS                          
         ZAP   DUB,PBDCOS                                                       
         CVB   R1,DUB                                                           
         M     R0,=F'200'                                                       
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         CVD   R1,DUB                                                           
         ZAP   PBDCOS,DUB+3(5)                                                  
*                                                                               
         GOTO1 PUTPRT                                                           
*                                                                               
         LA    R4,NEWSCNT                                                       
         CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         LA    R4,MAGSCNT                                                       
         B     PROC14                SKIP DMPREC                                
*                                                                               
         CP    0(8,R4),=P'10'                                                   
         BH    PROC14                                                           
         BAS   RE,DMPREC                                                        
PROC14   DS    0H                                                               
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         LA    R1,GROSS            ORDERED                                      
         LA    R2,RNOG                                                          
         LA    R3,3                                                             
         BAS   RE,BUYADD                                                        
         LA    R1,PGROSS           PAID                                         
         LA    R2,RNPG                                                          
         LA    R3,3                                                             
         BAS   RE,BUYADD                                                        
         LA    R1,BGROSS           BILLED                                       
         LA    R2,RNBG                                                          
         LA    R3,3                                                             
         BAS   RE,BUYADD                                                        
         B     PROC16             SKIP HEXOUTS                                  
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,GROSS,P+15,28,=C'N'                                  
         MVC   P+1(11),=C'NEW ORDERED'                                          
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,PGROSS,P+15,20,=C'N'                                 
         MVC   P+1(8),=C'NEW PAID'                                              
         BAS   RE,RPRT                                                          
         GOTO1 HEXOUT,DMCB,BGROSS,P+15,16,=C'N'                                 
         MVC   P+1(10),=C'NEW BILLED'                                           
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
*                                                                               
PROC16   AP    0(8,R4),=P'1'                                                    
         B     PROC3                                                            
*                                                                               
PROCEND  LA    R2,RUNTOTS                                                       
         LA    R1,REQTOTS                                                       
         BAS   RE,ROLL                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+5(14),=C'REQUEST TOTALS'                                       
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         MVC   P+10(3),=C'OLD'                                                  
         BAS   RE,RPRT                                                          
         LA    R4,TITLES                                                        
         LA    R2,REQTOTS                                                       
         LA    R3,9                                                             
PROCE5   MVC   P+1(15),0(R4)                                                    
         EDIT  (P8,0(R2)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         BAS   RE,RPRT                                                          
         LA    R2,8(R2)                                                         
         LA    R4,15(R4)                                                        
         BCT   R3,PROCE5                                                        
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P+10(3),=C'NEW'                                                  
         BAS   RE,RPRT                                                          
         LA    R4,TITLES                                                        
         LA    R2,REQTOTS+72                                                    
         LA    R3,9                                                             
PROCE7   MVC   P+1(15),0(R4)                                                    
         EDIT  (P8,0(R2)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         BAS   RE,RPRT                                                          
         LA    R2,8(R2)                                                         
         LA    R4,15(R4)                                                        
         BCT   R3,PROCE7                                                        
*                                                                               
         LA    R1,REQTOTS           CLEAR TOTALS                                
         LA    R2,18                                                            
PROCE9   ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R2,PROCE9                                                        
         B     EXIT                 DONE                                        
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
NEWSCNT  DS    PL8                                                              
         DC    CL15'NEWSPAPER BUYS'                                             
MAGSCNT  DS    PL8                                                              
         DC    CL15'NON-NEWS BUYS  '                                            
         DC    X'FF'                                                            
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         B     EXIT                                                             
******************************OLD CODE                                          
         MVI   FORCEHED,C'Y'                                                    
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
         BAS   RE,RPRT                                                          
         MVC   P+5(10),=C'RUN TOTALS'                                           
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         MVC   P+10(3),=C'OLD'                                                  
         BAS   RE,RPRT                                                          
         LA    R4,TITLES                                                        
         LA    R2,RUNTOTS                                                       
         LA    R3,9                                                             
RUNL15   MVC   P+1(15),0(R4)                                                    
         EDIT  (P8,0(R2)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         BAS   RE,RPRT                                                          
         LA    R2,8(R2)                                                         
         LA    R4,15(R4)                                                        
         BCT   R3,RUNL15                                                        
*                                                                               
         BAS   RE,RPRT                                                          
         MVC   P+10(3),=C'NEW'                                                  
         BAS   RE,RPRT                                                          
         LA    R4,TITLES                                                        
         LA    R2,RUNTOTS+72                                                    
         LA    R3,9                                                             
RUNL17   MVC   P+1(15),0(R4)                                                    
         EDIT  (P8,0(R2)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         BAS   RE,RPRT                                                          
         LA    R2,8(R2)                                                         
         LA    R4,15(R4)                                                        
         BCT   R3,RUNL17                                                        
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
         DC    CL15'GROSS'                                                      
         DC    CL15'AGENCY COM'                                                 
         DC    CL15'CASH DISC.'                                                 
         DC    CL15'BILLED GROSS'                                               
         DC    CL15'BILLED AGYCOM'                                              
         DC    CL15'BILLED CD'                                                  
         DC    CL15'PAID GROSS'                                                 
         DC    CL15'PAID AGYCOM'                                                
         DC    CL15'PAID CD'                                                    
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
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
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
REQTOTS  DS    0PL8                                                             
ROG      DS    PL8            OLD GROSS,AC,CD                                   
ROAC     DS    PL8                                                              
ROCD     DS    PL8                                                              
RBG      DS    PL8            OLD BILLED GROSS,AC,CD                            
RBAC     DS    PL8                                                              
RBCD     DS    PL8                                                              
RPG      DS    PL8            OLD PAID GROSS,AC,CD                              
RPAC     DS    PL8                                                              
RPCD     DS    PL8                                                              
*                                                                               
RNOG     DS    PL8                                                              
RNOAC    DS    PL8                                                              
RNOCD    DS    PL8                                                              
RNBG     DS    PL8                                                              
RNBAC    DS    PL8                                                              
RNBCD    DS    PL8                                                              
RNPG     DS    PL8                                                              
RNPAC    DS    PL8                                                              
RNPCD    DS    PL8                                                              
**                                                                              
RUNTOTS  DS    0PL8                                                             
TOG      DS    PL8                                                              
TOAC     DS    PL8                                                              
TOCD     DS    PL8                                                              
TBG      DS    PL8                                                              
TBAC     DS    PL8                                                              
TBCD     DS    PL8                                                              
TPG      DS    PL8                                                              
TPAC     DS    PL8                                                              
TPCD     DS    PL8                                                              
*                                                                               
TNOG     DS    PL8                                                              
TNOAC    DS    PL8                                                              
TNOCD    DS    PL8                                                              
TNBG     DS    PL8                                                              
TNBAC    DS    PL8                                                              
TNBCD    DS    PL8                                                              
TNPG     DS    PL8                                                              
TNPAC    DS    PL8                                                              
TNPCD    DS    PL8                                                              
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'129PPREP0202N05/01/02'                                      
         END                                                                    
