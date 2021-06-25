*          DATA SET SPREPFXHW  AT LEVEL 006 AS OF 01/12/01                      
*PHASE SPFX02H                                                                  
         TITLE 'SPFX02 - FIX X-18- ELEMS ON BUYS'                               
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PBUY                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* REQFRST                                                                       
REQF     DS    0H                                                               
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'11'                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'             EOF?                                       
         BE    REQFX                                                            
         CLI   DMCB+8,0                                                         
         BE    REQF20                                                           
         DC    H'0'                                                             
*                                                                               
REQF10   GOTO1 SEQ                                                              
         CLI   KEY,X'FF'             EOF?                                       
         BE    REQFX                                                            
         CLI   DMCB+8,0                                                         
         BE    REQF20                                                           
         DC    H'0'                                                             
*                                                                               
REQF20   CLI   KEY+10,X'80'        SKIP SPILL MKTS                              
         BE    REQF10                                                           
*                                                                               
         MVI   DMOUTBTS,X'FD'        DON'T GET DELETED RECORDS                  
         MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
*                                                                               
         USING BUYRECD,R3                                                       
         L     R3,AREC                                                          
         LA    R6,BDELEM                                                        
         XC    BUYFLAG,BUYFLAG       CLEAR THE FLAG                             
*                                                                               
REQF30   CLI   0(R6),0               END OF RECORD?                             
         BE    REQF10                                                           
*                                                                               
         CLI   0(R6),X'18'           X'18' ELEM?                                
         BNE   REQF40                NO -- CHECK FOR BUY ELEM                   
         TM    BUYFLAG,GOTBUY        YES -- DID I GET A BUY FIRST?              
         BO    REQF32                  YES -- DON'T PRINT RECORD                
*                                                                               
         BAS   RE,PRTBUY               NO -- PRINT RECORD                       
         B     REQF10                                                           
*                                                                               
REQF32   XI    BUYFLAG,GOTBUY        TURN BIT OFF                               
         B     REQF50                BUMP TO NEXT ELEMENT                       
*                                                                               
REQF40   CLI   0(R6),X'0B'           IS IT A BUY ELEMENT?                       
         BNE   *+8                   NO -- BUMP TO NEXT ELEMENT                 
         OI    BUYFLAG,GOTBUY        YES -- TURN BIT ON                         
*                                                                               
         CLI   0(R6),X'0C'           IS IT AN  X'OC' ELEMENT?                   
         BNE   *+8                   NO-- BUMP TO NEXT ELEMENT                  
         OI    BUYFLAG,GOTBUY        YES -- TURN BIT ON                         
*                                                                               
REQF50   ZIC   R0,1(R6)              LENGTH OF ELEMENT                          
         AR    R6,R0                 BUMP TO NEXT ELEMENT                       
         B     REQF30                                                           
*                                                                               
REQFX    GOTO1 AENDREQ                                                          
                                                                                
* PROCBUY                                                                       
PBUY     DS    0H                                                               
*                                                                               
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
REQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                          PRINT BUY                                  *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTBUY   NTR1                                                                   
*        SR    R0,R0                                                            
*        ICM   R0,3,13(R5)                                                      
*        GOTO1 PRNTBL,DMCB,=C'BUYREC',(R5),C'DUMP',(R0),=C'1D00'                
*                                                                               
         L     R5,ADBUY                                                         
         USING BUYRECD,R5                                                       
PRTB2    LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVC   P(2),BUYALPHA         PRINT THE AGENCY                           
         MVC   RCAGENCY,BUYALPHA                                                
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
*        MVC   PPRD,=C'POL'                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
*        CLI   QOPT5,C'Y'          PRINT OUT KEY FOR DEBUGGING                  
*        BNE   PRTB3                                                            
         GOTO1 HEXOUT,DMCB,BDCIND2,PIND2,1                                      
         GOTO1 HEXOUT,DMCB,KEY,PKEY,18                                          
PRTB3    GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
COUNT    DS    F                                                                
ELCODE   DS    X                                                                
BUYFLAG  DS    X                                                                
GOTBUY   EQU   X'01'                                                            
SAVEKEY  DS    XL13                                                             
SAVEKEY2 DS    XL18                                                             
SVKEY    DS    XL10                AM(1),CLT(2),PRD(1),MKSTA(5),EST(1)          
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PIND2    DS    CL1                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPFXHW 01/12/01'                                      
         END                                                                    
