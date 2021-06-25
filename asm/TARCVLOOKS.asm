*          DATA SET TARCVLOOKS AT LEVEL 063 AS OF 05/01/02                      
*PHASE TALOOKA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE TINVCON                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'TARCVLOOK - SEARCH RECOVERY FILE AND PRINT'                     
RCVLOOK  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,RCVLOOK,VREGSAVE                                               
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RCVLOOK+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,RCVLOOK          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
*****    OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
         L     R1,VMASTC                                                        
         USING MASTD,R1                                                         
         CLI   MCWRITE,C'N'        IF WRITE = NO                                
         BNE   *+8                                                              
         MVI   WRITSTAT,C'N'       SET WRITE =NO                                
         DROP  R1                                                               
*                                                                               
         MVC   TITLE(20),=C'TALENT RECOVERY LOOK'                               
*                                                                               
*****    MVC   MID1(57),=C' AGENCY   INVOICE  STAFF     COMML ID     US         
*****          E  CYCLE DATES'                                                  
*****    MVC   MID2(57),=C' ------   -------  -----     --------     --         
*****          -  -----------'                                                  
         B     IN2                                                              
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      GET   RECVIN,RCVREC                                                    
*                                                                               
******   CLI   RFILTY,X'71'        TEST TALDIR                                  
******   BE    *+12                                                             
         CLI   RFILTY,X'72'        TEST TALFIL                                  
         BNE   IN2                                                              
*                                                                               
         CLC   RDATE,=X'5E0604'     TEST CORRECT DATE                           
         BNE   IN2                                                              
         CLC   RTIME(3),=X'014300'  TEST WROTE TO FILE AFTER 10:30 PM           
         BL    IN2                                                              
         CLC   RTIME(3),=X'015400'  AND BEFORE 11:40PM                          
         BH    IN2                                                              
*                                                                               
         CLI   RKEY,TLFTCDQ        TEST FOR FTRACK REC                          
         BNE   IN5                                                              
         CLI   RRECTY,3            IF ADD                                       
         BNE   IN2                                                              
         CLI   WRITSTAT,C'N'       IF WRITING TO FILE                           
         BE    PRINTFT                                                          
         LA    R6,RKEY             R6=A(RECORD)                                 
         GOTO1 =V(DATAMGR),DMCB,=CL8'ADDREC',=CL8'TALFIL',0,(R6),DMWORK         
         B     PRINTFT                                                          
*                                                                               
IN5      CLI   RKEY,TLCACDQ        TEST FOR CAST REC                            
         BNE   IN2                                                              
         CLI   RRECTY,1            IF COPY                                      
         BE    *+12                                                             
         CLI   RRECTY,2            OR CHANGE                                    
         BNE   IN2                                                              
**NO-OP* MVC   AIO,=A(RCVREC)                                                   
**NO-OP* GOTO1 PUTREC                                                           
**NO-OP* GOTO1 =V(DATAMGR),DMCB,=CL8'PUTREC',KEY+,RCVREC,DMWORK                 
**NO-OP* PUT   RECVOUT,RCVREC      WRITE TO OUTPUT TAPE                         
         B     PRINTCA                                                          
         SPACE                                                                  
PRINTFT  LA    R4,RKEY                                                          
         USING TLFTD,R4                                                         
         MVC   P(9),TLFTSSN                                                     
         MVC   WORK(6),TLFTINV                                                  
         GOTO1 =V(TINVCON),DMCB,WORK,P+12,V(DATCON)                             
         GOTO1 =V(HEXOUT),DMCB,TLFTCOM,P+22,L'TLFTCOM,=C'TOG'                   
         GOTO1 =V(PRINTER)                                                      
         AP    OUTFCNT,=P'1'                                                    
         B     IN2                                                              
         SPACE                                                                  
PRINTCA  LA    R4,RKEY                                                          
         USING TLCAD,R4                                                         
         MVC   P(9),TLCASSN                                                     
         GOTO1 =V(HEXOUT),DMCB,TLCACOM,P+22,L'TLCACOM,=C'TOG'                   
         GOTO1 =V(PRINTER)                                                      
         AP    OUTCCNT,=P'1'                                                    
         B     IN2                                                              
***************                                                                 
         CLI   RRECTY,1            IF COPY                                      
         BNE   *+14                                                             
         MVC   SAVEKEY,RKEY        SAVE KEY OF COPY                             
         B     IN2                                                              
*                                                                               
         CLI   RRECTY,2            IF CHANGE                                    
         BNE   IN2                                                              
         CLC   RKEY,SAVEKEY        AND KEY CHANGED FROM TIME OF COPY            
         BE    IN10                                                             
         OC    CPYKEY,CPYKEY       AND NOT FIRST TIME IN                        
         BZ    IN8                                                              
         CLC   CPYKEY,RKEY         CHECK VALID CHANGE                           
         BNE   *+14                                                             
         CLC   CHGKEY,SAVEKEY                                                   
         BE    IN6                                                              
         BAS   RE,PRTSAVE          PRINT FIRST COPY/CHANGE                      
         BAS   RE,PRTCUR           PRINT CURRENT COPY/CHANGE                    
*                                                                               
IN6      XC    CPYKEY,CPYKEY                                                    
         XC    CHGKEY,CHGKEY                                                    
         B     IN2                                                              
*                                                                               
IN8      MVC   CPYKEY,SAVEKEY      SAVE FIRST COPY KEY                          
         MVC   CHGKEY,RKEY         SAVE FIRST CHANGE KEY                        
         B     IN2                                                              
*                                                                               
IN10     OC    CPYKEY,CPYKEY       IF SAVED COPY OR CHANGE                      
         BNZ   *+14                                                             
         OC    CHGKEY,CHGKEY                                                    
         BZ    IN2                                                              
         BAS   RE,PRTSAVE          PRINT THEM                                   
         XC    CPYKEY,CPYKEY                                                    
         XC    CHGKEY,CHGKEY                                                    
         B     IN2                                                              
*                                                                               
PRTSAVE  NTR1                                                                   
         MVC   P(8),=CL8'ORG COPY'                                              
         GOTO1 =V(HEXOUT),DMCB,CPYKEY,P+10,L'CPYKEY,=C'TOG'                     
         GOTO1 =V(PRINTER)         AND PRINT IT                                 
*                                                                               
         MVC   P(8),=CL8'ORG CHG'                                               
         GOTO1 =V(HEXOUT),DMCB,CHGKEY,P+10,L'CHGKEY,=C'TOG'                     
         GOTO1 =V(PRINTER)         AND PRINT IT                                 
         B     XIT                                                              
         SPACE 2                                                                
PRTCUR   NTR1                                                                   
         MVC   P(8),=CL8'CUR COPY'                                              
         GOTO1 =V(HEXOUT),DMCB,SAVEKEY,P+10,L'SAVEKEY,=C'TOG'                   
         GOTO1 =V(PRINTER)         AND PRINT IT                                 
*                                                                               
         MVC   P(8),=CL8'CUR CHG '                                              
         GOTO1 =V(HEXOUT),DMCB,RKEY,P+10,L'RKEY,=C'TOG'                         
         GOTO1 =V(PRINTER)         AND PRINT IT                                 
         B     XIT                                                              
*                                                                               
*********************************************************                       
*                                                                               
         BAS   RE,PRINTIT          PRINT IT OUT                                 
         AP    CHGCOUNT,=P'1'      ADD TO COUNT                                 
         B     IN2                                                              
*                                                                               
         CLC   MYKEY(MYKEYLQ),RKEY   TEST MATCH ON KEY                          
         BE    IN10                                                             
         CLC   MYKEY1(MYKEY1LQ),RKEY TEST MATCH ON KEY                          
         BE    IN10                                                             
         CLC   MYKEY2(MYKEY2LQ),RKEY                                            
         BNE   IN2                                                              
*                                                                               
         BAS   RE,PRINTIT          PRINT OUT RECORD                             
         B     IN2                                                              
*                                                                               
******   CLI   RRECTY,3            TEST FOR ADD                                 
******   BNE   IN2                                                              
*                                                                               
*******  CLC   RTIME(3),=X'001300' TEST ADDED BEFORE 9:30 AM                    
*******  BH    IN2                                                              
*                                                                               
******   BAS   RE,PRNTINV          PRINT INVOICE DETAILS                        
******   B     IN2                                                              
*                                                                               
MYKEY    DC    X'90',13X'00',C'ABSL  ',2X'00',X'E66CF7FDABFF'                   
MYKEYLQ  EQU   *-MYKEY                                                          
MYKEY1   DC    X'A800',C'499504976'                                             
MYKEY1LQ EQU   *-MYKEY1                                                         
MYKEY2   DC    X'D000',C'1508  ',2X'00',C'QAAN1177    ',X'00',C'JR810B'         
MYKEY2LQ EQU   *-MYKEY2                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
PRNTINV  NTR1                                                                   
         LA    R4,RKEY                                                          
         USING TLIND,R4                                                         
         MVC   P+1(6),TLINAGY                                                   
         MVC   WORK(6),TLININV                                                  
         XC    WORK(6),=6X'FF'                                                  
         GOTO1 =V(TINVCON),DMCB,WORK,P+10,V(DATCON)                             
*                                                                               
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TAIND,R4                                                         
         MVC   P+19(8),TAINPST                                                  
*                                                                               
         LA    R4,RKEY                                                          
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TACOD,R4                                                         
         MVC   P+29(12),TACOCID                                                 
*                                                                               
         LA    R4,RKEY                                                          
         MVI   ELCODE,TAPDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PINV20                                                           
         USING TAPDD,R4                                                         
         MVC   P+42(3),TAPDUSE                                                  
         OC    TAPDCYCS,TAPDCYCS                                                
         BZ    PINV20                                                           
         GOTO1 =V(DATCON),DMCB,(X'11',TAPDCYCS),(8,P+47)                        
*                                                                               
PINV20   GOTO1 =V(PRINTER)         AND PRINT IT                                 
*                                                                               
         AP    OUTCNT,=P'1'        ADD TO OUTPUT COUNT                          
         B     XIT                                                              
         EJECT                                                                  
PRINTIT  NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,RECVHDR-4      GET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,0,RECVHDR-4,C'DUMP',(R0),=C'1D'                  
         AP    OUTCNT,=P'1'        ADD TO OUTPUT COUNT                          
         B     XIT                                                              
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
******   CLOSE RECVOUT                                                          
*                                                                               
         GOTO1 =V(PRINTER)                                                      
         EDIT  (P4,OUTFCNT),(10,P+1),ZERO=NOBLANK,COMMAS=YES                    
         MVC   P+12(16),=CL16'FTRACK RECORDS '                                  
         GOTO1 =V(PRINTER)                                                      
         EDIT  (P4,OUTCCNT),(10,P+1),ZERO=NOBLANK,COMMAS=YES                    
         MVC   P+12(9),=C'CAST RECS'                                            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               MACRF=GM,EODAD=ENDIN                                             
         SPACE 1                                                                
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=2048,            X        
               MACRF=PM,BLKSIZE=8200,BUFNO=2                                    
         EJECT                                                                  
DMCB     DS    6F                                                               
DMWORK   DS    12D                                                              
WRITSTAT DC    C' '                                                             
MYSEQ    DC    F'0'                                                             
OUTCNT   DC    PL4'0'                                                           
OUTFCNT  DC    PL4'0'                                                           
OUTCCNT  DC    PL4'0'                                                           
CHGCOUNT DC    PL4'0'                                                           
DUB      DS    D                                                                
SAVEKEY  DS    CL(L'RKEY)                                                       
CPYKEY   DS    CL(L'RKEY)                                                       
CHGKEY   DS    CL(L'RKEY)                                                       
WORK     DS    XL64                                                             
DATADISP DC    Y(TLRCELEM-TLRCD)                                                
ELCODE   DS    XL1                                                              
*                                                                               
         DS    0D                                                               
         DC    C'*RCVREC*'                                                      
RCVREC   DC    F'0'                VAR REC LEN                                  
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
RKEY     DS    0CL32                                                            
         DS    2100C                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063TARCVLOOKS05/01/02'                                      
         END                                                                    
