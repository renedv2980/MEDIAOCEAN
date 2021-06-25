*          DATA SET SPREPFX02Y AT LEVEL 037 AS OF 09/14/00                      
*          DATA SET SPREPFX02A AT LEVEL 040 AS OF 04/04/00                      
*PHASE SPFX02Y                                                                  
         TITLE 'SPFX02 - SCAN FILE FOR UNHUNG EXPLODED NETWORK BUYS'            
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
         CLI   MODE,CLTFRST                                                     
         BE    FX20                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
FX10     MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'      SET TO IGNORE 'RECORD IS DELETED'            
         XC    MYKEY,MYKEY                                                      
         B     EXIT                                                             
*                                                                               
FX20     L     RE,=A(TAB)          CLEAR EST/LINE TABLE                         
         L     RF,=A(TABX-TAB)                                                  
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)                                                     
         MVI   KEY+3,X'FF'            FORCE PRD = POL (MKT 0)                   
         GOTO1 HIGH                                                             
*                                                                               
FX22     CLC   KEY(6),KEYSAVE      SAME A-M/CLT/POL/MKT0                        
         BNE   FX100               NO - EXIT                                    
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         GOTO1 MSUNPK,DMCB,KEY+4,DUB,NETEBC    SAVE UNPACKED STATION            
         MVC   P(22),=C'NOW PROCESSING NETWORK'                                 
         MVC   P+23(4),NETEBC                                                   
         GOTO1 REPORT                                                           
*                                                                               
FX24     CLC   KEY(9),LASTPRT      A-M/CLT/PRD/MKT/STA/EST                      
         BE    FX26                                                             
         BAS   RE,PRTKEY                                                        
         MVC   LASTPRT,KEY                                                      
*                                                                               
FX26     SR    RE,RE                                                            
         IC    RE,KEY+9            GET ESTIMATE                                 
         MHI   RE,256                                                           
         A     RE,=A(TAB)                                                       
         SR    R0,R0                                                            
         IC    R0,KEY+11           GET LINE                                     
         AR    RE,R0               POINT TO TABLE ENTRY                         
         STC   R0,0(RE)            STORE LINE NUMBER                            
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(8),MYKEY        A-M/CLT/PRD/MKT/STA                          
         BE    FX24                                                             
         EJECT                                                                  
* GET SEQNUM FOR THIS NETWORK                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D91'                                                  
         MVC   KEY+2(2),AGENCY                                                  
         GOTO1 HIGH                                                             
*                                                                               
FX30     CLC   KEY(4),KEYSAVE                                                   
         BE    FX32                                                             
         MVC   P(27),=C'** ERROR ** NO SUCH NETWORK'                            
         GOTO1 REPORT                                                           
         BAS   RE,PRTKEY                                                        
         B     FX60                                                             
*                                                                               
FX32     CLC   KEY+5(4),NETEBC                                                  
         BE    FX34                                                             
         GOTO1 SEQ                                                              
         B     FX30                                                             
*                                                                               
FX34     MVC   NETSEQ,KEY+4                                                     
*                                                                               
         EJECT                                                                  
* NOW READ EXPLODED BUYS FILTERING ON NETSEQ                                    
*                                                                               
FX40     MVC   KEY(4),MYKEY        A-M/CLT/PRD                                  
         MVC   KEY+4(2),=X'0001'   FORCE NON-ZERO MKT                           
*                                                                               
FX42     GOTO1 HIGH                                                             
         B     FX44X                                                            
*                                                                               
FX44     GOTO1 SEQ                                                              
*                                                                               
FX44X    CLC   KEY(4),KEYSAVE      SAME A-M/CLT                                 
         BNE   FX60                                                             
*                                                                               
         TM    KEY+13,X'80'        TEST KEY IS DELETED                          
         BO    FX44                                                             
*                                                                               
         CLI   KEY+10,0            TEST SPILL POINTER                           
         BNE   FX44                YES - IGNORE                                 
*                                                                               
         CLC   KEY+8(1),NETSEQ     RIGHT NETWORK                                
         BE    FX48                YES                                          
         BH    FX46                HIGH - NEXT STATION                          
*                                                                               
         MVC   KEY+8(1),NETSEQ     READ FOR THIS SEQNUM                         
         XC    KEY+9(4),KEY+9                                                   
         B     FX42                                                             
*                                                                               
FX46     MVC   KEY+9(4),=4X'FF'                                                 
         B     FX42                                                             
*                                                                               
FX48     SR    RE,RE                                                            
         IC    RE,KEY+9            GET ESTIMATE                                 
         MHI   RE,256                                                           
         A     RE,=A(TAB)                                                       
         SR    R0,R0                                                            
         IC    R0,KEY+11                                                        
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   FX50                                                             
* UNHUNG BUYLINE FOUND !                                                        
         LA    RE,P+2+(PCOM-PLINED)                                             
         MVC   0(6,RE),=C'UNHUNG'                                               
         BAS   RE,PRTKEY                                                        
*                                                                               
         OI    KEY+13,X'80'        DELETE THE DIRECTORY                         
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 GETBUY                                                           
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         OI    BUYREC+15,X'80'                                                  
         GOTO1 PUTBUY                                                           
         DROP  R6                  FUCKHEAD                                     
*                                                                               
FX50     B     FX44                                                             
         EJECT                                                                  
FX60     MVC   KEY,MYKEY           RESTORE LAST NETWORK KEY                     
         MVC   KEY+9(4),=6X'FF'                                                 
         GOTO1 HIGH                                                             
*                                                                               
         L     RE,=A(TAB)          CLEAR EST/LINE TABLE                         
         L     RF,=A(TABX-TAB)                                                  
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     FX22                                                             
*                                                                               
*                                                                               
FX100    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* S/R TO PRINT BUY RECORD KEYS                                                  
*                                                                               
PRTKEY   NTR1                                                                   
*                                                                               
         LA    R6,KEY                                                           
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),AGY                                                      
*                                                                               
         MVC   PMED,QMED                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BNE   *+10                                                             
         MVC   PSTA+5(2),=C'TV'                                                 
*                                                                               
         MVC   PNET,NETEBC                                                      
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKEY+11                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,PKEY,13,=C'TOG'                                  
*                                                                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
ELCODE   DS    X                                                                
FLAG     DS    X                                                                
         DS    0D                                                               
         DC    CL8'*NETSEQ*'                                                    
NETSEQ   DS    X                                                                
NETEBC   DS    CL8                                                              
         DS    0D                                                               
         DC    CL8'**MYKEY*'                                                    
MYKEY    DS    XL13                                                             
         DS    0D                                                               
         DC    CL8'*LASTPRT'                                                    
LASTPRT  DS    XL13                                                             
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
LASTNET  DC    XL3'00'                                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
TAB      DS    256XL256                                                         
TABX     EQU   *                                                                
                                                                                
*                                                                               
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
*                                                                               
PNET     DS    CL3                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PKEY     DS    CL30                BUY LINE KEY                                 
         DS    CL1                                                              
PCOM     DS    CL10                                                             
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPREPFX02Y09/14/00'                                      
         END                                                                    
