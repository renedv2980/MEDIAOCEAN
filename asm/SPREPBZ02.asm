*          DATA SET SPREPBZ02  AT LEVEL 028 AS OF 11/01/00                      
*PHASE SPBZ02A                                                                  
         SPACE 1                                                                
*=================================================================*             
* DO NOT RUN THIS UNDER ANY CIRCUMSTANCE UNTIL FURTHER NOTICE     *             
*=================================================================*             
         TITLE 'SPBZ02 - NON-POL BUY CONVERSION'                                
SPBZ02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
MAXELS   EQU   169                 MAXIMUM REGELS IN A BUY                      
         NMOD1 0,SPBZ02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPBZ02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    FX10                                                             
         CLI   MODE,STAFRST                                                     
         BE    FX300                                                            
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   FX02                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
FX02     CLI   MODE,REQLAST                                                     
         BE    FX200                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* PROCBUY                                                                       
FX10     DS    0H                                                               
         MVC   KEYSAVE,KEY         SAVE CURRENT KEY                             
         CLI   KEY+10,0                                                         
         BNE   EXIT                                                             
         TM    KEY+13,X'80'        TEST DELETED                                 
         BO    EXIT                                                             
*                                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         TM    15(R6),X'80'        TEST DELETED                                 
         BO    EXIT                                                             
         MVI   FIXED,C'N'                                                       
*                                                                               
FX12     DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX14                                                             
         OI    KEY+13,X'80'                                                     
         GOTO1 WRITE               DELETE DIRECTORY                             
         OI    15(R6),X'80'        DELETE RECORD                                
         GOTO1 PUT                                                              
         NI    15(R6),X'7F'        UNSET DELETE BIT FOR ADD LATER               
*                                                                               
         USING BUYRECD,R6                                                       
FX14     MVC   BDMASPRD(1),BUYKPRD                                              
         MVC   MYPRD,BUYKPRD                                                    
         MVC   MYSLN,BDSEC                                                      
*                                                                               
         BAS   RE,PRTELS           PRINT 'BEFORE' RECORD                        
*                                                                               
FX20     L     R6,ADBUY                                                         
         LA    R6,BDELEM                                                        
*                                                                               
FX22     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF RECORD                                
         BE    FX100                                                            
         CLI   0(R6),X'06'                                                      
         BE    FX30                                                             
         CLI   0(R6),X'07'                                                      
         BNE   FX22                                                             
         EJECT                                                                  
*--------------------------------------------------------------*                
* CREATE 0B ELEMENTS AND MINUS OTO (0C) ELEMS AS NEEDED        *                
*--------------------------------------------------------------*                
         SPACE 1                                                                
FX30     XC    SVELS,SVELS                                                      
*                                                                               
         MVC   SVEL1,0(R6)             SAVE ELEMENT                             
         GOTO1 RECUP,DMCB,ADBUY,(R6)   AND DELETE FROM RECORD                   
*                                                                               
         CLI   0(R6),X'07'             TEST NEXT ELEM IS MINUS OTO              
         BNE   FX34                                                             
         TM    6(R6),X'80'                                                      
         BZ    FX34                                                             
* NEXT ELEMENT IS A -OTO                                                        
         MVC   SVEL2,0(R6)             SAVE MINUS OTO                           
*                                                                               
FX32     GOTO1 RECUP,DMCB,ADBUY,(R6)   AND DELETE FROM RECORD                   
*                                                                               
FX34     CLI   0(R6),X'10'         REMOVE ELEMS X'10'-X'1F'                     
         BL    FX36                                                             
         CLI   0(R6),X'1F'                                                      
         BH    FX36                                                             
         B     FX32                GO DELETE IT                                 
*                                                                               
FX36     BAS   RE,BLDELS           BUILD NEW ELEMENTS                           
*                                                                               
         SR    R7,R7               R7 HAS POSITIVE SPOT COUNT                   
         SR    R8,R8               R8 HAS MINUS SPOT COUNT                      
*                                                                               
         ICM   R7,1,SVEL1+7        NUMBER OF POSITIVE SPOTS                     
         BNZ   *+8                                                              
         LA    R7,1                                                             
*                                                                               
         CLI   SVEL2,0             TEST FOR MINUS ELEMENT                       
         BNE   FX40                YES                                          
         SPACE 1                                                                
*=============================================================*                 
* NO MINUS SPOTS - JUST ADD 0B ELEMENTS                       *                 
*=============================================================*                 
         SPACE 1                                                                
FX38     GOTO1 RECUP,DMCB,(C'S',ADBUY),ELEM1,(C'R',(R6))                        
         CLI   8(R1),0                                                          
         BE    BFP                                                              
         BCT   R7,FX38                                                          
         B     FX20                                                             
         EJECT                                                                  
*=============================================================*                 
* THERE ARE MINUS SPOTS - MISS THE LAST 0B ELEMENTS           *                 
*=============================================================*                 
         SPACE 1                                                                
FX40     DS    0H                                                               
         ICM   R8,1,SVEL2+7                                                     
         BNZ   *+8                                                              
         LA    R8,1                                                             
*                                                                               
         CR    R7,R8               NEED MORE PLUS THAN MINUS                    
         BNL   *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX42     CR    R7,R8               MARK THE LAST ONES MISSED                    
         BH    *+8                                                              
         OI    ELEM1+6,X'40'       SET MISSED FLAG                              
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',ADBUY),ELEM1,(C'R',(R6))                        
         CLI   8(R1),0             TEST DID NOT FIT                             
         BE    BFP                                                              
*                                                                               
         CR    R7,R8               TEST DOING MINUS SPOTS YET                   
         BH    FX44                NOT YET                                      
*                                  NEED TO ADD MINUS OTO                        
         SR    R0,R0                                                            
         IC    R0,1(R6)            POINT AFTER ELEM JUST INSERTED               
         AR    R6,R0                                                            
*                                                                               
         GOTO1 (RF),(R1),,ELEM2,(C'R',(R6))   AND ADD THE MINUS OTO             
         CLI   8(R1),0             TEST DID NOT FIT                             
         BE    BFP                                                              
*                                                                               
FX44     SR    R0,R0                                                            
         IC    R0,1(R6)            POINT AFTER ELEM JUST INSERTED               
         AR    R6,R0                                                            
         BCT   R7,FX42             AND CONTINUE                                 
*                                                                               
         B     FX20                                                             
*                                                                               
BFP      MVI   FIXED,C'X'          SET RECUP PROBLEMO                           
         L     R0,MYERRS                                                        
         AHI   R0,1                                                             
         ST    R0,MYERRS                                                        
         BAS   RE,PRTBUY                                                        
         B     FX102                                                            
         EJECT                                                                  
BLDELS   NTR1                                                                   
         XC    ELEMS,ELEMS                                                      
         LA    R3,ELEM1                                                         
         USING REGELEM,R3                                                       
         IC    RF,SVEL1            GET OLD ELEMENT CODE                         
         LA    RF,5(RF)                                                         
         STC   RF,RCODE                                                         
         MVI   RLEN,14                                                          
         MVC   RDATE(4),SVEL1+2     DATE/PAY DATE                               
         MVC   RPPRD,MYPRD                                                      
         MVC   RPTIME,MYSLN                                                     
         MVC   RPPAYSEQ,SVEL1+8    CLEARANCE SEQNUM                             
*                                                                               
         LA    R3,ELEM2                                                         
         USING REGELEM,R3                                                       
         IC    RF,SVEL2            GET OLD ELEMENT CODE                         
         LA    RF,5(RF)                                                         
         STC   RF,RCODE                                                         
         MVI   RLEN,14                                                          
         MVC   RDATE(4),SVEL2+2    ELEM DATE AND PAY DATE                       
         OI    RSTATUS,X'80'       SET MINUS ELEMENT                            
         MVC   RPPRD,MYPRD                                                      
         MVC   RPTIME,MYSLN                                                     
         MVC   RPPAYSEQ,SVEL2+8    CLEARANCE SEQNUM                             
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
FX100    MVI   FIXED,C'Y'          PRINT 'AFTER'                                
         BAS   RE,PRTELS                                                        
*                                                                               
         BAS   RE,CHKMAXEL         SEE IF PERHAPS HAVE TOO MANY ELEMS           
*                                                                               
         L     R6,ADBUY                                                         
         MVI   BUYKPRD,X'FF'                                                    
         ZIC   RE,HIGHPOL          SET CURRENT POL LINE NUMBER                  
         LA    RE,1(RE)                                                         
         STC   RE,HIGHPOL                                                       
         CLI   HIGHPOL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                TOO MANY LINE NUMBERS                        
         STC   RE,BUYKEY+10        SET LINE NUMBER IN RECORD                    
         BAS   RE,PRTBUY                                                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX102                                                            
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,(R6),DMWORK           
FX102    L     R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,COUNT                                                         
* RESTORE DIRECTORY FOR SEQUENTIAL READING                                      
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         NI    DMOUTBTS,X'FD'      DO NOT TEST X'02'                            
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         B     EXIT                                                             
                                                                                
FX200    DS    0H                                                               
         MVC   P(15),=C'RECORDS CHANGED'                                        
         EDIT  COUNT,(5,P+20)                                                   
         MVC   P2(12),=C'ERRORS FOUND'                                          
         EDIT  MYERRS,(5,P2+20)                                                 
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================*               
* STAFRST - FIND HIGHEST POL LINE NUMBER FOR THIS STATION       *               
*===============================================================*               
         SPACE 1                                                                
FX300    MVI   HIGHPOL,0                                                        
         XC    KEY+10(3),KEY+10    SAVE A-M/CLT/PRD/MKT/STA/EST                 
         MVI   KEY+3,X'FF'         SET PRD=POL                                  
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         NI    DMOUTBTS,X'FD'      DO NOT TEST X'02'                            
         GOTO1 HIGH                                                             
*                                                                               
FX302    CLC   KEY(10),KEYSAVE                                                  
         BNE   FX310                                                            
         MVC   HIGHPOL,KEY+11      SAVE HIGH LINE NUMBER                        
         GOTO1 SEQ                                                              
         B     FX302                                                            
*                                                                               
FX310    MVC   KEY,SVBUYKEY        RESTORE KEY FOR CONTROLLER                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
HIGHPOL  DC    X'00'                                                            
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
PRTB2    LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
*                                                                               
         BAS   RE,FINDPRD                                                       
         MVC   PPRD,0(R1)                                                       
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   PSTA+5,C'T'                                                      
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         ZIC   R0,KEY+11           GET CURRENT NON-POL LINE NUMBER              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLINOLD,DUB                                                      
*                                                                               
         CLI   FIXED,C'X'          TEST RECUP PROBLEM                           
         BNE   *+10                                                             
         MVC   PKEY(14),=C'RECUP PROBLEMO'                                      
         CLI   FIXED,C'E'          TEST TOO MANY ELEMENTS                       
         BNE   *+10                                                             
         MVC   PKEY(15),=C'TOO MANY REGELS'                                     
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
FINDPRD  L     R1,ADCLT                                                         
         LA    R1,CLIST-CLTHDR(R1)                                              
FINDPRD2 CLC   SVBUYKEY+3(1),3(R1)                                              
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    FINDPRD2                                                         
         DCHO                                                                   
         EJECT                                                                  
CHKMAXEL NTR1                                                                   
         L     R6,ADBUY                                                         
         MVI   ELCODE,X'0B'                                                     
         SR    R4,R4                                                            
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
CHKMAX2  BCTR  R4,0                                                             
         BAS   RE,NEXTEL                                                        
         BE    CHKMAX2                                                          
         LPR   R4,R4                                                            
         CHI   R4,MAXELS                                                        
         BNH   EXIT                                                             
         L     R0,MYERRS                                                        
         AHI   R0,1                                                             
         ST    R0,MYERRS                                                        
         MVI   FIXED,C'E'          SET 'ELEMENT' PROBLEM                        
         B     EXIT                                                             
         EJECT                                                                  
*===============================================================*               
* SUBROUTINE PRINTS BUY ELEMENTS                                *               
*===============================================================*               
         SPACE 1                                                                
PRTELS   NTR1                                                                   
         CLI   QOPT1,C'Y'          PRINT ELEMENT TRACE                          
         BNE   PRTELX                                                           
*                                                                               
PRTEL0   MVC   P(5),=C'AFTER'                                                   
         CLI   FIXED,C'Y'                                                       
         BE    *+10                                                             
         MVC   P(6),=C'BEFORE'                                                  
* PRINT KEY AND BDELEM                                                          
         L     R6,ADBUY                                                         
         GOTO1 HEXOUT,DMCB,(R6),P+8,24,=C'TOG'                                  
         LA    R6,24(R6)                                                        
         GOTO1 (RF),(R1),(R6),P2+8,40                                           
         GOTO1 (RF),(R1),40(R6),P3+8,30                                         
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
PRTEL2   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    PRTELX                                                           
         CLI   0(R6),X'06'         FIND FIRST REGEL                             
         BL    PRTEL2                                                           
*                                                                               
PRTEL4   LH    R4,MAXCHARS         SET MAX CHARS TO PRINT                       
         LA    R5,P+8              SET FIRST PRINT POSITION                     
*                                                                               
PRTEL6   IC    R0,1(R6)            GET ELEMENT LENGTH                           
         SR    R4,R0                                                            
         BP    PRTEL10                                                          
         AR    R4,R0               RESTORE R4 FOR NEXT TEST                     
         CH    R4,MAXCHARS         TEST ELEM WON'T EVER FIT                     
         BNE   PRTEL8              IT MIGHT, BUT THERE'S STUFF HERE             
         LH    R0,MAXCHARS         ELSE JUST PRINT MAXCHARS                     
         B     PRTEL10                                                          
*                                                                               
PRTEL8   GOTO1 REPORT                                                           
         B     PRTEL4                                                           
*                                                                               
PRTEL10  GOTO1 HEXOUT,DMCB,(R6),(R5),(R0),=C'TOG'                               
         A     R5,DMCB+16          ADD OUTPUT LENGTH                            
         LA    R5,1(R5)                                                         
*                                                                               
PRTEL12  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   PRTEL6                                                           
*                                                                               
         CH    R4,MAXCHARS         TEST ANYTHING MORE TO PRINT                  
         BE    PRTELX                                                           
         GOTO1 REPORT                                                           
*                                                                               
PRTELX   XIT1                                                                   
PRTCOUNT DC    PL4'0'                                                           
MAXCHARS DC    H'60'               MAX INPUT CHARS PER LINE                     
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'*ELEMS**'                                                    
ELEMS    DS    0XL64                                                            
ELEM1    DS    XL32                                                             
ELEM2    DS    XL32                                                             
         DC    CL8'**SVELS*'                                                    
SVELS    DS    0XL32                                                            
SVEL1    DS    XL10                                                             
         DS    XL6                                                              
SVEL2    DS    XL10                                                             
         DS    XL6                                                              
*                                                                               
COUNT    DS    F                                                                
MYERRS DS      F                                                                
ELCODE   DS    X                                                                
MYPRD    DS    X                                                                
MYSLN    DS    X                                                                
FIXED    DS    C                                                                
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
INRECS   DC    F'0',CL20'RECORDS IN'                                            
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
         SPACE 2                                                                
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
SPILLELS DS    1024C                                                            
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
SDEFRECD DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PLINOLD  DS    CL3                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028SPREPBZ02 11/01/00'                                      
         END                                                                    
