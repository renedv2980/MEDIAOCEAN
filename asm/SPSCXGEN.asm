*          DATA SET SPSCXGEN   AT LEVEL 020 AS OF 11/15/10                      
*PHASE SPSCXGNB                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE DMDMGRL                                                                
                                                                                
         TITLE 'SPSCXGEN - GENERATING CX REQUEST CARDS'                         
SPSCXGEN CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         ENTRY COMFACS                                                          
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,SPSCXGEN,=V(REGSAVE)                                           
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         LA    RE,SPSCXGEN          SET FOR STXITER                             
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
         EJECT                                                                  
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0   INIT SORTER                 
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (SCXGOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*============================================================                   
*** PROCESS INPUT FILE ***                                                      
*============================================================                   
                                                                                
QSPTFIL  EQU   X'21'                                                            
COPY     EQU   1                                                                
CHG      EQU   2                                                                
ADD      EQU   3                                                                
*                                                                               
IN2      LA    R0,DM$RECVHDR-4                                                  
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   DM$RFILTY,QSPTFIL   SPTFILE ?                                    
         BNE   IN2                                                              
         CLI   DM$RRECTY,X'02'     RECORD CHANGED?                              
         BE    *+12                                                             
         CLI   DM$RRECTY,X'03'     RECORD ADDED?                                
         BNE   IN2                                                              
*                                                                               
         LA    R2,DM$RECVHDR+24                                                 
         USING BUYRECD,R2                                                       
*                                                                               
         CLI   BUYKEY,X'10'        TEST BUYREC                                  
         BL    IN2                 NO                                           
*                                                                               
         TM    BUYKEY,X'03'        TEST NETWORK                                 
         BNO   IN2                                                              
*                                                                               
         OC    BUYKEY+4(2),BUYKEY+4  TEST MARKET 0                              
         BNZ   IN2                   NO - IGNORE                                
*                                                                               
         TM    BUYKEY+15,X'80'     TEST DELETED                                 
         BO    IN2                 YES - SKIP                                   
*                                                                               
         LLC   R0,BUYKBUY          ASSUME 1-BYTE LINE NUM                       
         TM    BUYKEY+15,BUYRLN2   TEST 2-BYTE LINE NUMS                        
         BO    *+8                                                              
         STCM  R0,3,BUYKBUY        SET AS 2-BYTE LINE NUM ALWAYS                
*                                                                               
         MVC   SORTAGYA,BUYALPHA   SET ALPHA AGY CODE                           
         MVC   SORTBUY,BUYKEY                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         B     IN2                                                              
         EJECT                                                                  
ENDIN    DS    0H                                                               
         CLOSE (RECVIN,)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         XC    SORTREC,SORTREC                                                  
*                                                                               
GETSORT  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,DMCB+4                                                     
         BZ    GETSORTX                                                         
*                                                                               
         CLC   SORTREC(15),0(R2)   TEST SAME DATA                               
         BE    GETSORT             YES - SKIP                                   
         MVC   SORTREC,0(R2)       MOVE DATA SO WE CAN SEE IT                   
*                                                                               
         LA    R0,SORTREC                                                       
         PUT   SCXGOUT,(R0)        WRITE TO FILE                                
         B     GETSORT                                                          
*                                                                               
GETSORTX DS    0H                                                               
         CLOSE (SCXGOUT,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
ENDPROG  XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
SCXGOUT  DCB   DDNAME=SCXGOUT,DSORG=PS,RECFM=FB,LRECL=16,              X        
               BLKSIZE=3200,MACRF=PM                                            
*                                                                               
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=24'                                    
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,16,A),FORMAT=BI,WORK=1'                      
*                                                                               
         DS    0D                                                               
WORK     DS    XL64                                                             
         DC    CL8'**DMCB**'                                                    
DMCB     DS    6F                                                               
DMWORK   DS    24F                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'**KEY***'                                                    
KEY      DS    XL48                                                             
KEYSAVE  DS    XL48                                                             
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
*                                                                               
FILSAVE  DS    CL5                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'*SORTREC'                                                    
SORTREC  DS    XL32                                                             
         ORG   SORTREC                                                          
SORTAGYA DS    CL2                                                              
SORTBUY  DS    XL13                                                             
         ORG                                                                    
*                                                                               
         DS    0D                                                               
CARD     DS    CL80                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'**UTL **'                                                    
UTL      DC    F'0',X'00'                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'02' NO RECOVERY                                  
         ORG                                                                    
                                                                                
*                                                                               
COMFACS  DS    0D                                                               
CDMGR    DC    V(DATAMGR)                                                       
CCALLOFF DC    V(CALLOFF)                                                       
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RECVLEN  DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
         SPACE 1                                                                
RDATA    DS    6100X                                                            
         PRINT OFF                                                              
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDSYSELD                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPSCXGEN  11/15/10'                                      
         END                                                                    
