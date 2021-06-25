*          DATA SET PPREP02023 AT LEVEL 020 AS OF 05/01/02                      
*PHASE PP02023,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTLIST PROGRAM'                                       
*                                                                               
*        THIS PROGRAM LISTS CLIENT RECORD PROFILES, ETC.                        
*                                                                               
*                                                                               
*        QOPT1 = Y    LIST ONLY ONE AGENCY                                      
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
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   CLTCNT,=P'0'    CLIENT HEADER COUNT                              
         ZAP   OFFCNT,=P'0'   CLIENTS USING OFFICES                             
         ZAP   AFFCNT,=P'0'   CLIENTS USING ACC OFFICES                         
         ZAP   ONECNT,=P'0'   CLIENTS USING ACC OFFICES - ONE CHAR              
         LA    R0,PCLTREC                                                       
         ST    R0,ADCLT                                                         
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      START AT AGENCY                              
         MVI   KEY+3,X'02'         CLIENTS                                      
*                                                                               
         MVI   SKEY,X'FF'                                                       
         CLI   QOPT1,C'Y'          ONE AGENCY ONLY ?                            
         BNE   *+10                NO                                           
         MVC   SKEY(2),KEY         YES - SAVE AGENCY FOR TEST                   
*                                                                               
AGYC2    GOTO1 HIGH                                                             
         B     AGYC4                                                            
*                                                                               
AGYC3    DS    0H                                                               
         GOTO1 SEQ                                                              
AGYC4    DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE                                  
         BE    EXIT                                                             
         CLC   KEY(2),SKEY         TEST FOR ONE AGENCY ONLY                     
         BH    EXIT                END OF ONE AGENCY RUN                        
         CLI   KEY+3,X'02'                                                      
         BH    NEXTAM                                                           
         BL    AGYC3                                                            
*                                                                               
AGYC6    DS    0H                                                               
         GOTO1 GETCLI                                                           
         AP    CLTCNT,=P'1'                                                     
*                                                                               
         CLI   PCLTOFF,0                                                        
         BNH   *+10                                                             
         AP    OFFCNT,=P'1'                                                     
*                                                                               
         CLC   PCLTAOFC,=X'0000'    CHECK FOR ACC OFFICE                        
         BE    AGYCRPT                                                          
         AP    AFFCNT,=P'1'                                                     
         CLI   PCLTAOFC+1,C' '                                                  
         BH    *+10                                                             
         AP    ONECNT,=P'1'         ONE CHARACTER ACC OFFICE CODE               
AGYCRPT  MVC   P1(2),PCLTKAGY                                                   
         MVC   P1+12(1),PCLTKMED                                                
         MVC   P1+19(3),PCLTKCLT                                                
         MVC   P1+30(1),PCLTOFF                                                 
*        GOTO1 HEXOUT,DMCB,PCLTOFF,P1+30,L'PCLTOFF,=C'N'                        
         MVC   P1+40(2),PCLTAOFC                                                
         OC    PCLTACCA,PCLTACCA      ACC AGENCY                                
         BZ    AGYC5                                                            
         MVI   P1+42,C'/'                                                       
         MVC   P1+43(2),PCLTACCA                                                
*                                                                               
*        GOTO1 HEXOUT,DMCB,PCLTAOFC,P1+40,L'PCLTAOFC,=C'N'                      
AGYC5    DS    0H                                                               
         MVC   P1+54(10),PCLTPROF                                               
         MVC   P1+65(10),PCLTPROF+10                                            
         MVC   P1+76(10),PCLTPROF+20                                            
         MVC   P1+87(2),PCLTPROF+30                                             
         BAS   RE,RPRT                                                          
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
CLTCNT   DS    PL8                                                              
         DC    CL15'CLIENTS READ'                                               
OFFCNT   DS    PL8                                                              
         DC    CL15'PCLTOFF USED'                                               
AFFCNT   DS    PL8                                                              
         DC    CL15'PCLTAOFC USED'                                              
ONECNT   DS    PL8                                                              
         DC    CL15'ONE CHAR ACC O'                                             
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
         MVC   HEAD8(6),=C'AGENCY'                                              
         MVC   HEAD9(6),=C'------'                                              
         MVC   HEAD8+10(5),=C'MEDIA'                                            
         MVC   HEAD9+10(5),=C'-----'                                            
         MVC   HEAD8+19(6),=C'CLIENT'                                           
         MVC   HEAD9+19(6),=C'------'                                           
         MVC   HEAD8+30(6),=C'OFFICE'                                           
         MVC   HEAD9+30(6),=C'------'                                           
         MVC   HEAD8+40(10),=C'ACC OFFICE'                                      
         MVC   HEAD9+40(10),=C'----------'                                      
         MVC   HEAD7+54(35),=C'CLIENT PROFILE --------------------'             
         MVC   HEAD8+54(10),=C'1234567890'                                      
         MVC   HEAD9+54(10),=C'0--------1'                                      
         MVC   HEAD8+65(10),=C'1234567890'                                      
         MVC   HEAD9+65(10),=C'1--------2'                                      
         MVC   HEAD8+76(10),=C'1234567890'                                      
         MVC   HEAD9+76(10),=C'2--------3'                                      
         MVC   HEAD8+87(2),=C'12'                                               
         MVC   HEAD9+87(2),=C'3-'                                               
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
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020PPREP0202305/01/02'                                      
         END                                                                    
