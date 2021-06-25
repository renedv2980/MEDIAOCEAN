*          DATA SET PPREP0302U AT LEVEL 002 AS OF 05/01/02                      
*PHASE PP0302U,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM LISTS PUB LIST RECORDS WITH ZERO PUBS                          
*                                                                               
*     QOPT1=Y  DELETE THE ABOVE RECORDS                                         
*     QOPT5=Y  DUMP 1ST 10 RECORDS DELETED                                      
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
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'         RECORDS READ                                 
         ZAP   ZEROCNT,=P'0'       RECS WITH NO PUBS IN LIST                    
         ZAP   NONECNT,=P'0'       RECS WITH NO ELEMENTS                        
         ZAP   DELCNT,=P'0'        RECS ACTUALLY DELETED                        
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                  FIRST BUY FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCWRITE,C'N'                                                     
         CLI   QOPT1,C'Y'          DELETE RECORD ?                              
         BNE   PROC1                                                            
         MVI   RCWRITE,C'Y'                                                     
PROC1    LA    R0,PUBREC           USE PUBREC FOR I/O                           
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'17'          PUB LIST RECORDS                            
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
PROC3    GOTO1 SEQ                                                              
         MVI   DELSW,0                                                          
PROC5    CLC   KEY(4),KEYSAVE      JUST CHECK AGY/MEDIA RECORD CODE             
         BNE   PROC80              END OF MEDIA GO DO NEXT                      
         AP    INCNT,=P'1'                                                      
         GOTO1 GETPRT              GET THE PUB LIST REC                         
*                                                                               
         CLI   KEY+10,1            FIRST RECORD OF LIST ?                       
         BE    PROC10              YES                                          
         ZICM  RE,PUBREC+25,2      RECORD LENGTH                                
         CH    RE,=H'33'           ANY ELEMENTS IN RECORD ?                     
         BH    PROC3               YES - NEXT NEXT RECORD                       
         AP    NONECNT,=P'1'                                                    
         MVI   DELSW,1                                                          
         MVC   P+25(17),=C'** NO ELEMENTS **'                                   
         B     PROC25              DELETE THIS RECORD                           
*                                                                               
PROC10   DS    0H                                                               
         LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'10'         PUB LIST DATE ELEMENT                        
         BE    PROC20                                                           
         DC    H'0'                DIE - 'LINE' 1 MUST HAVE THIS ELEM           
*                                                                               
PROC20   DS    0H                                                               
         USING PLISDTEL,R2                                                      
         CLC   PLISNPBS,=X'0000'    ZERO PUBS IN LIST ?                         
         BH    PROC3               NO - NEXT RECORD                             
         AP    ZEROCNT,=P'1'                                                    
*NOP*    EDIT  PLISNPBS,(3,P+34),ALIGN=LEFT   NUMBER OF PUBS IN LIST            
         DROP  R2                                                               
*                                                                               
PROC25   DS    0H                                                               
         LA    R4,KEY                                                           
         USING PPDUM00,R4          DSECT FOR PUB LIST REC                       
         MVC   P(2),PLISKAGY       AGENCY                                       
         MVC   P+5(1),PLISKMED     MEDIA                                        
         MVC   P+8(3),PLISKCLT     CLIENT                                       
         MVC   P+13(3),PLISKCOD    LIST CODE                                    
PROC30   EDIT  PLISKLIN,(2,P+19),FILL=0      'LINE' NUMBER                      
         OI    KEY+25,X'80'                                                     
         CLI   RCWRITE,C'Y'        DELETE RECORD ?                              
         BNE   PROC40              NO                                           
         MVC   P+45(15),=C'*** DELETED ***'                                     
         AP    DELCNT,=P'1'                                                     
         GOTO1 WRT                                                              
*                                                                               
PROC40   DS    0H                                                               
         BAS   RE,RPRT             PRINT THE LINE                               
*                                                                               
         CLI   QOPT5,C'Y'          DUMP RECORDS ?                               
         BNE   PROC50              NO                                           
         CLI   DELSW,1             DELETING NO ELEMENT RECORD ?                 
         BNE   PROC42              NO                                           
         CP    NONECNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC50              YES                                          
         B     PROC45              NO                                           
PROC42   CP    ZEROCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC50              YES                                          
PROC45   MVC   P+65(9),=C'** KEY **'                                            
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT             PRINT THE LINE                               
         BAS   RE,DMPREC                                                        
*                                                                               
PROC50   DS    0H                                                               
         CLI   DELSW,1             DELETING "NO ELEMENT" RECORD ?               
         BE    PROC3               YES - NEXT RECORD                            
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                 NEXT SEQ REC                                 
         CLC   KEY(PLISKLIN-PLISREC),KEYSAVE       SAME LIST ?                  
         BNE   PROC5               NO                                           
         AP    INCNT,=P'1'                                                      
*                                                                               
         MVC   P+5(10),=C'** SAME **'                                           
         AP    ZEROCNT,=P'1'                                                    
         B     PROC30              GO FLAG FOR DELETION                         
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    PROC80X                                                          
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'         SKIP TO NEXT AGY/MED                         
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    PROC80X                                                          
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'17'         PUB LIST REC                                 
         B     PROC2                                                            
*                                                                               
PROC80X  DS    0H                                                               
         MVI   MODE,RUNLAST                                                     
         B     EXIT                END OF RUN                                   
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
         MVI   RCSUBPRG,60                                                      
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
*****    L     RF,ALTLREC                                                       
*****    XC    0(50,RF),0(RF)                                                   
*****    GOTO1 SEQPUB                                                           
*****    CLC   KEY(9),PUBKEY                                                    
*****    BNE   NPX                                                              
*****    GOTO1 GETLTL                                                           
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
         LA    R2,31                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(31),0(R5)                                                   
         TR    WORK(31),TRTAB                                                   
         MVC   P+85(31),WORK                                                    
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
         DC    CL15'LIST RECS READ'                                             
ZEROCNT  DS    PL8                                                              
         DC    CL15'NO PUBS IN LIST'                                            
NONECNT  DS    PL8                                                              
         DC    CL15'NO ELEMS IN REC'                                            
DELCNT   DS    PL8                                                              
         DC    CL15'FLAGGED FOR DEL'                                            
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
DELSW    DS    CL1                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
CHKGST   DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
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
PCATD    DSECT                                                                  
       ++INCLUDE PCATELEM                                                       
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREP0302U05/01/02'                                      
         END                                                                    
