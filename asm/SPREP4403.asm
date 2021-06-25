*          DATA SET SPREP4403  AT LEVEL 014 AS OF 02/14/02                      
*PHASE SP4403A,+0                                                               
         TITLE 'SP4403 - SP44 SUBCONTROLER'                                     
SP4403   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4403                                                         
*                                                                               
         L     RA,0(R1)                                                         
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
         USING SPWORKD,RA,R8                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   PAGE,=H'1'                                                       
         XC    BQSTART,BQSTART     USED TO SAVE RTG SRVC FOR SPILL              
         CLI   QOPT1,C'N'          NETWORKS                                     
         BE    NETW                                                             
         CLI   QOPT1,C'S'          SHOWS                                        
         BE    SHOW                                                             
         CLI   QOPT1,C'L'          SPILLS                                       
         BE    SPL                                                              
*                                                                               
         MVC   P(21),=C'INVALID REPORT OPTION'                                  
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
NETW     DS    0H                                                               
*                                                                               
         MVI   SVEST,0                                                          
         CLC   QEST,=C'ALL'                                                     
         BE    NETW5                                                            
         CLC   QEST,=C'   '                                                     
         BE    NETW5                                                            
         PACK  DUB,QEST(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,SVEST                                                         
*                                                                               
NETW5    CLC   QCLT,=C'ALL'                                                     
         BNE   NET1                                                             
         CLC   QUESTOR,=CL12'CONTROL'                                           
         BNE   NET1                                                             
         MVC   QCLT,=C'   '        TURNAROUND                                   
         MVC   QEST,=C'   '                                                     
*                                  DON'T DO ALL CLTS                            
NET1     DS    0H                                                               
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGY                                                     
         CLC   =C'ALL',QMKT        TEST ALL NETS                                
         BE    *+10                                                             
         MVC   KEY+4(4),QMKT                                                    
         CLC   =C'ALL',QCLT                                                     
         BE    NET4                                                             
         CLC   =C'   ',QCLT                                                     
         BE    NET4                                                             
         GOTO1 CLPACK,DMCB,QCLT,SVCLT                                           
*                                                                               
NET2     MVC   KEY+8(2),SVCLT                                                   
         OC    SVEST,SVEST                                                      
         BZ    *+10                                                             
         MVC   KEY+10(1),SVEST                                                  
*                                                                               
NET4     GOTO1 HIGH                                                             
*                                                                               
NET5     CLC   KEY(4),KEYSAVE                                                   
         BNE   NETX                END OF AGY                                   
         CLC   =C'ALL',QMKT                                                     
         BE    *+14                                                             
         CLC   KEY+4(4),QMKT       DOING ONE NETWORK                            
         BNE   NETX                                                             
         CLC   =C'ALL',QCLT                                                     
         BE    NET6                                                             
         CLC   KEY+8(2),SVCLT                                                   
         BE    NET5C                                                            
         BL    NET2                                                             
*        NEXT NETWORK                                                           
         MVC   KEY+8(2),=X'FFFF'                                                
         B     NET4                                                             
*                                                                               
*                                                                               
NET5C    CLC   QEST,=C'ALL'                                                     
         BE    NET6                                                             
         OC    SVEST,SVEST     SEE IF IGNORING ESTIMATE EXCEPTIONS              
         BZ    NET5E                                                            
         CLC   KEY+10(1),SVEST                                                  
         BE    NET6                                                             
         BL    NET2                                                             
*        GO TO NEXT NETWORK                                                     
         MVC   KEY+8(2),=X'FFFF'                                                
         B     NET4                                                             
*                                                                               
NET5E    CLI   KEY+10,0             SEE IF ESTIMATE EXCEPTION                   
         BE    NET6                 NO - OK                                     
         MVC   KEY+10(2),=X'FFFF'    SKIP TO NEXT CLIENT                        
         B     NET4                                                             
*                                                                               
*        PROCESS RECORD                                                         
NET6     MVC   SVBUYKEY,KEY        SAVE KEY                                     
         MVI   MODE,PROCNET                                                     
         GOTO1 GO                                                               
         MVC   KEY,SVBUYKEY        RESTORE KEY                                  
         MVI   KEY+11,X'FF'            FORCE NEXT ESTIMATE                      
         CLC   =C'ALL',QEST            SEE IF DOING ALL ESTIMATES               
         BE    NET4                                                             
         MVC   KEY+10(2),=X'FFFF'      FORCE NEXT CLT                           
         CLC   =C'ALL',QCLT                                                     
         BE    NET4                                                             
         MVC   KEY+8(2),=X'FFFF'   FORCE NEXT NETW                              
         CLC   =C'ALL',QMKT                                                     
         BE    NET4                                                             
NETX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
SHOW     DS    0H                                                               
         CLC   QSTA,=5C' '                                                      
         BNE   *+10                                                             
         MVC   QSTA(3),=C'ALL'                                                  
         MVC   KEY(2),=X'0D12'                                                  
         MVC   KEY+2(2),AGY                                                     
         CLC   =C'ALL',QMKT        TEST DOING ALL NETWORKS                      
         BE    *+10                                                             
         MVC   KEY+4(4),QMKT                                                    
         CLC   =C'ALL',QSTA                                                     
         BE    *+10                                                             
         MVC   KEY+8(4),QSTA                                                    
*                                                                               
SHW4     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SHWX                END OF AGY                                   
         CLC   =C'ALL',QMKT                                                     
         BE    *+14                                                             
         CLC   KEY+4(4),QMKT                                                    
         BNE   SHWX                                                             
         CLC   =C'ALL',QSTA        TEST ALL SHOWS                               
         BE    SHW6                                                             
         CLC   KEY+8(4),QSTA                                                    
         BNE   SHWX                                                             
*                                                                               
SHW6     DS    0H                  PROCESS RECORD                               
         MVC   SVBUYKEY,KEY        SAVE KEY                                     
         MVI   MODE,PROCSHW                                                     
         GOTO1 GO                                                               
         MVC   KEY,SVBUYKEY        RESTORE                                      
         MVI   KEY+12,X'FF'                                                     
         CLC   =C'ALL',QSTA        TEST ALL SHOWS                               
         BE    SHW4                                                             
*                                                                               
         MVC   KEY+8(2),=X'FFFF'                                                
         CLC   =C'ALL',QMKT        TEST ALL NETWS                               
         BE    SHW4                                                             
*                                                                               
SHWX     B     EXIT                                                             
         EJECT                                                                  
SPL      DS    0H                                                               
         CLC   QSTA,=5C' '                                                      
         BNE   *+10                                                             
         MVC   QSTA(3),=C'ALL'                                                  
         MVC   KEY(2),=X'0D13'                                                  
         MVC   KEY+2(2),AGY                                                     
         MVI   KEY+4,C'0'                                                       
*                                  CHECKING RATING SERVICE HERE                 
         CLC   =C'NSI',QMKT        TEST DOING NSI                               
         BE    SPL2                                                             
         CLC   =C'ALL',QMKT                                                     
         BE    SPL2                                                             
         MVI   KEY+4,C'1'          BBM                                          
*                                                                               
SPL2     CLC   =C'ALL',QSTA        TEST ALL STATIONS                            
         BE    SPL4                                                             
SPL3     MVC   KEY+5(5),QSTA                                                    
*                                                                               
SPL4     CLC   =C'ALL',QCLT                                                     
         BE    SPL6                                                             
         CLC   =C'   ',QCLT                                                     
         BE    SPL6                                                             
         GOTO1 CLPACK,DMCB,QCLT,SVCLT                                           
SPL5     MVC   KEY+10(2),SVCLT                                                  
*                                                                               
SPL6     GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SPLX                END OF AGY                                   
*                                                                               
         CLC   =C'ALL',QMKT        TEST ALL RTG SRVCS                           
         BE    SPL8                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   SPLX                                                             
*                                                                               
SPL8     CLC   =C'ALL',QSTA        TEST ALL STATIONS                            
         BE    SPL10                                                            
         CLC   KEY+5(5),QSTA                                                    
         BE    SPL10                                                            
         BL    SPL3                                                             
         B     SPL14                                                            
*                                                                               
SPL10    CLC   =C'ALL',QCLT                                                     
         BE    SPL12                                                            
         CLC   KEY+10(2),SVCLT                                                  
         BE    SPL12                                                            
         BL    SPL5                                                             
         B     SPL14                                                            
*                                                                               
SPL12    DS    0H                                                               
         CLI   QMED,C'R'                                                        
         BNE   SPL12A                                                           
         CLI   KEY+9,C'A'                STATION MUST HAVE MEDIA C'A'           
         BE    SPL13                     OR                                     
         CLI   KEY+9,C'F'                STATION MUST HAVE MEDIA C'F'           
         BNE   SPL14                     ELSE SKIP                              
         B     SPL13                                                            
*                                                                               
SPL12A   CLI   KEY+9,C'A'                TV - SKIP RADIO STATIONS               
         BE    SPL14                                                            
         CLI   KEY+9,C'F'                                                       
         BE    SPL14                                                            
*                                                                               
SPL13    DS    0H                                                               
         MVC   SVBUYKEY,KEY                                                     
         MVI   MODE,PROCSPL                                                     
         GOTO1 GO                                                               
*                                                                               
         MVC   KEY,SVBUYKEY                                                     
SPL14    MVI   KEY+12,X'FF'        NEXT CLT                                     
         CLC   =C'ALL',QCLT                                                     
         BE    SPL6                                                             
*                                                                               
SPL16    MVC   KEY+10(2),=X'FFFF'       NEXT STA                                
         CLC   =C'ALL',QSTA                                                     
         BE    SPL6                                                             
*                                                                               
         MVC   KEY+5(2),=X'FFFF'      NEXT RTG SRVC                             
         CLC   =C'ALL',QMKT                                                     
         BE    SPL6                                                             
*                                                                               
SPLX     B     EXIT                                                             
*                                                                               
PROCNET  EQU   1                                                                
PROCSHW  EQU   2                                                                
PROCSPL  EQU   3                                                                
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREP4403 02/14/02'                                      
         END                                                                    
