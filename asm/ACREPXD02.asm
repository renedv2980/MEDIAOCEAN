*          DATA SET ACREPXD02  AT LEVEL 148 AS OF 05/01/02                      
*PHASE ACXD02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'SALARY ELEMENT FIX'                                             
ACXD02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXD**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXDD,RC                                                         
         CLI   MODE,RUNFRST                                                     
         BNE   ACC05                                                            
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         ZAP   CHACNT,=P'0'             RECORD CHANGED CNT TO O                 
         ZAP   ELECNT,=P'0'             ELEMTS CHANGED                          
*                                                                               
         MVI   FCRDACC,C'Y'             READ ACCOUNTS ONLY                      
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDHIST,C'N'                                                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
ACC05    CLI   MODE,REQFRST                                                     
         BNE   ACC10                                                            
*        GOTO1 DATCON,DMCB,(0,QSTART),(1,START3)                                
*        GOTO1 DATCON,DMCB,(0,QSTART),(2,START2)                                
*        GOTO1 DATCON,DMCB,(0,QEND),(1,END3)                                    
*        GOTO1 DATCON,DMCB,(0,QEND),(2,END2)                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
ACC10    CLI   MODE,PROCACC                                                     
         BNE   ACC990                                                           
         MVI   CHANGED,C'N'                                                     
         L     R3,ADACC                                                         
         USING ACKEYD,R3                                                        
*                                                                               
ACC20    CLC   =C'1R',1(R3)             1R ACCOUNT                              
         BNE   XIT                                                              
*                                                                               
*        BAS   RE,DMPGET                DUMP REC COMING IN                      
         L     R4,ADACC                                                         
         AH    R4,DATADISP              GO GET FIRST ELEMENT                    
ACC110   DS    0H                                                               
         CLI   0(R4),X'52'              IS IT A 52 ELEMENT                      
         BE    ACC112                   NEXT ELEMENT PLEASE                     
*        CLI   0(R4),X'5D'              IS IT A 5D ELEMENT                      
*        BE    ACC115                   NEXT ELEMENT PLEASE                     
*        CLI   0(R4),X'5E'              IS IT A 5E ELEMENT                      
*        BE    ACC125                   NEXT ELEMENT PLEASE                     
         B     ACC140                                                           
*                                                                               
ACC112   DS    0H                                                               
         USING ACSALRYD,R4                                                      
         CLI   ACSALBEG,X'97'                                                   
         BNL   ACC140              DON'T DELETE                                 
*        CLI   ACSALEND,X'95'                                                   
*        BH    ACC140                                                           
*        TM    ACSALTYP,X'50'                                                   
*        BZ    ACC140                                                           
*        CLI   ACSALBAS,C'M'                                                    
*        BNE   ACC140                                                           
         B     ACC135                                                           
*                                                                               
ACC115   DS    0H                                                               
*        USING ACBSALD,R4                                                       
*        OC    ACBSENDT,ACBSENDT   OPEN ENDED?                                  
*        BZ    ACC140              DON'T DELETE                                 
*                                                                               
*        CLI   ACBSENDT,X'93'                                                   
*        BH    ACC140                                                           
*        B     ACC135                                                           
*                                                                               
ACC125   DS    0H                                                               
*        USING ACCLPCTD,R4                                                      
*        OC    ACCLPCEN,ACCLPCEN   OPEN ENDED?                                  
*        BZ    ACC140              DON'T DELETE                                 
*                                                                               
*        GOTO1 DATCON,DMCB,(2,ACCLPCEN),(1,START3)                              
*        CLI   START3,X'93'                                                     
*        BH    ACC140                                                           
*        B     ACC135                                                           
*                                                                               
ACC135   MVI   CHANGED,C'Y'                                                     
         AP    ELECNT,=P'1'                                                     
         MVI   0(R4),X'FF'                                                      
*                                                                               
ACC140   ZIC   R0,1(R4)                      LENGTH OF EL IN R0                 
         AR    R4,R0                         BUMP UP R4 ADDR BY LENGTH          
         CLI   0(R4),0                       END OF RECORD                      
         BNE   ACC110                        NO -- CHECK NEXT ELEMENT           
*                                                                               
         CLI   CHANGED,C'Y'        WAS THIS RECORD CHANGED                      
         BNE   XIT                 NO                                           
         BAS   RE,DMPGET                DUMP REC COMING IN                      
         L     R4,ADACC                                                         
         GOTO1 DELEL,DMCB,(X'FF',(R4)),0     DELETE MARKED EL'S                 
*                                                                               
         AP    CHACNT,=P'1'                  ADD TO REC CHANGED COUNT           
         BAS   RE,DMPPUT                                                        
         BAS   RE,PRNTIT2                                                       
         CLI   RCWRITE,C'N'                  WRITE=NO                           
         BE    *+8                                                              
         MVI   MODE,WRITACC                  WRITE THE RECORD BACK              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*ACC200   CLI   MODE,PROCTRNS                                                   
*         B     ACC990                                                          
*         L     R4,ADTRANS                                                      
*         USING TRANSD,R4                                                       
*         CLI   0(R4),X'44'                                                     
*         BNE   XIT                                                             
*         L     R3,ADACC                                                        
*         USING ACKEYD,R3                                                       
*         CLC   ACKEYACC+1(9),=C'SVP202520'                                     
*         BE    *+6                                                             
*         DC    H'0'                WRONG ACCOUNT                               
*         L     R3,ADTRANS                                                      
*         SH    R3,DATADISP                                                     
*         CLC   ACKEYCON,CONTRA                                                 
*         BNE   XIT                                                             
*         CLI   TRNSANAL,C'1'                                                   
*         BNE   XIT                                                             
*         BAS   RE,DMPGET                                                       
*         MVI   TRNSANAL,C'7'                                                   
*         MVI   MODE,WRITRANS                                                   
*         BAS   RE,DMPPUT                                                       
*         AP    CHACNT,=P'1'                                                    
*         B     XIT                                                             
*                                                                               
*CONTRA   DC    X'7B'                                                           
*         DC    C'T'                                                            
*         DC    X'EB'                                                           
*         DC    CL12'FP'                                                        
         EJECT                                                                  
ACC990   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         BAS   RE,PRNTIT2                                                       
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
PRNTIT   NTR1                                                                   
         EDIT  DELRCD,(12,P+17),COMMAS=YES                                      
         MVC   P+1(15),=C'DELETED RECORDS'                                      
         GOTO1 ACREPORT                                                         
         EDIT  OUTRCD,(12,P+17),COMMAS=YES                                      
         MVC   P+1(15),=C'OUTPUT RECORDS '                                      
         GOTO1 ACREPORT                                                         
         EDIT  TRNRCD,(12,P+17),COMMAS=YES                                      
         MVC   P+1(16),=C'TRANS CHANGED  '                                      
         GOTO1 ACREPORT                                                         
         EDIT  DELDBT,(12,P+17),2                                               
         MVC   P+1(15),=C'DELETED DEBITS '                                      
         GOTO1 ACREPORT                                                         
         EDIT  DELCRT,(12,P+17),2                                               
         MVC   P+1(15),=C'DELETED CREDITS'                                      
*        GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 1                                                                
PRNTIT2  NTR1                                                                   
         CLI   MODE,RUNLAST                                                     
         BNE   PRNT02                                                           
         MVC   P+1(21),=C'TOTAL RECORDS CHANGED'                                
         EDIT  CHACNT,(12,P+25),COMMAS=YES                                      
         MVC   PSECOND+1(21),=C'TOTAL ELEMENT CHANGED'                          
         LA    R2,PSECOND+25                                                    
         EDIT  ELECNT,(12,0(R2)),COMMAS=YES                                     
         B     PRNT03                                                           
PRNT02   MVC   HEAD8+32(32),=C'ACCTS WITH 52 5E 5D ELE  CHANGES'                
         MVC   HEAD9+32(32),=C'--------------------------------'                
         MVC   P+1(14),ACKEYACC+1            1R ACCOUNT NUMBER                  
PRNT03   GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
*        AP    PDUMP,=P'1'                                                      
*        CP    PDUMP,MAXDUMP                                                    
*        BH    XIT                                                              
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
         SPACE 1                                                                
DUMP     L     R3,ADACC                                                         
         USING ACKEYD,R3                                                        
         SR    R8,R8                                                            
         ICM   R8,3,ACLENGTH                                                    
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'G',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',=C'ACCOUNT '),(R2),(R3)                         
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
         SPACE 1                                                                
DELRCD   DC    PL6'0'                                                           
OUTRCD   DC    PL6'0'                                                           
TRNRCD   DC    PL6'0'                                                           
DELDBT   DC    PL6'0'                                                           
DELCRT   DC    PL6'0'                                                           
CHACNT   DC    PL6'0'              COUNT OF RECORDS CHANGED                     
ELECNT   DC    PL6'0'              COUNT OF ELEMTS CHANGED                      
CHANGED  DC    CL1'N'              WRITE FLAG                                   
         SPACE 1                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'200'                                                         
*                                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ACXDD    DSECT                                                                  
TODAY2   DS    CL2                                                              
START3   DS    CL3                                                              
END3     DS    CL3                                                              
START2   DS    CL2                                                              
END2     DS    CL2                                                              
DMPSW    DS    CL1                                                              
ACTIVITY DS    CL1                                                              
         SPACE 2                                                                
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'148ACREPXD02 05/01/02'                                      
         END                                                                    
