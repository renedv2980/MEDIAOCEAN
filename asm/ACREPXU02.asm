*          DATA SET ACREPXU02  AT LEVEL 074 AS OF 08/16/00                      
*PHASE ACXU02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'FIND MISSING POSTING FOR BATCH ITEM HDRS'                       
ACXU02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXU**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXUD,RC                                                         
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   REQ                                                              
         ZAP   TRECCNT,=P'0'                                                    
         B     XIT                                                              
REQ      CLI   MODE,REQFRST                                                     
         BNE   RUNL                                                             
         ZAP   RECCNT,=P'0'                                                     
         ZAP   DRTOT,=P'0'                                                      
         ZAP   CRTOT,=P'0'                                                      
*                                                                               
         XC    DKEY,DKEY                                                        
         LA    R5,DKEY                                                          
         USING TBARECD,R5                                                       
         MVI   TBAKTYP,TBAKTYPQ    SET KEY FOR TRANS BATCH RECORDS              
*        MVC   TBAKCPY,QCOMPANY                                                 
*                                                                               
         BAS   RE,HIGH                                                          
         B     *+8                                                              
RS00     BAS   RE,SEQ                                                           
RS00A    MVC   DKEY,DIR                                                         
         MVC   KEYSV,DKEY                                                       
         MVI   HIT,0                                                            
*                                                                               
         LA    R5,DIR                                                           
         CLI   TBARECD,TBAKTYPQ    HAS TO BE TRANS BATCH RECORD                 
         BNE   RECAP               I WON'T BE BACK                              
*                                                                               
*        CLC   TBAKCPY,QCOMPANY    MATCHING COMPANY?                            
*        BNE   RECAP                                                            
*                                                                               
         OC    TBAKTSEQ,TBAKTSEQ   SKIP HEADER                                  
         BNZ   RS01                                                             
         TM    TBAKHSTA,TBAHSUPD   IGNORE UDPATED BATCHES                       
         BZ    RS00                                                             
         SR    R1,R1                                                            
         MVC   TBAKTSEQ,=X'FFFF'                                                
         MVC   DKEY,DIR                                                         
         BAS   RE,HIGH                                                          
         B     RS00A                                                            
RS01     DS    0H                                                               
         TM    TBAKESTA,TBAESLDE   LOGICALLY DELETED?                           
         BO    RS00                                                             
*        CLI   TBAKBTYP,61                                                      
*        BNE   RS00                                                             
*        CLC   TBAKADDT,=X'3B18'                                                
*        BNE   RS00                                                             
*                                                                               
         AP    TRECCNT,=P'1'                                                    
         L     R3,AIO2                                                          
         BAS   RE,GET                                                           
         L     R5,AIO2                                                          
         USING ASKELD,R2                                                        
         LA    R2,TBARFST                                                       
RS02     CLI   0(R2),0             EOR?                                         
         BE    RS07                                                             
         CLI   ASKEL,ASKELQ                                                     
         BE    RS08                                                             
RS06     SR    R1,R1               BUMP TO NEXT                                 
         IC    R1,ASKLN                                                         
         AR    R2,R1                                                            
         B     RS02                                                             
*                                                                               
RS07     DS    0H                                                               
         TM    HIT,HITBAD                                                       
         BZ    *+14                                                             
         AP    RECCNT,=P'1'                                                     
         BAS   RE,REPORTIT                                                      
*                                                                               
         MVC   DKEY,KEYSV                                                       
         BAS   RE,HIGH                                                          
         B     RS00                                                             
*                                                                               
         USING TRNRECD,R5                                                       
RS08     DS    0H                                                               
         XC    DKEY,DKEY                                                        
         SR    R1,R1                                                            
         IC    R1,ASKLN                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+4                                                           
         MVC   DKEY(0),ASKKEY      SEE IF TRANS KEY THERE                       
         BAS   RE,HIGH                                                          
         CLC   DKEY,DIR                                                         
         BE    RS06                                                             
         OI    HIT,HITBAD                                                       
         B     RS06                                                             
*                                                                               
         EJECT                                                                  
RUNL     CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
         GOTO1 ACREPORT                                                         
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         MVC   P(11),=C'** WRONG **'                                            
         EDIT  RECCNT,(14,P+20),0,MINUS=YES                                     
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P(11),=C'** TOTAL **'                                            
         EDIT  TRECCNT,(14,P+20),0,MINUS=YES                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
RECAP    DS    0H                                                               
         GOTO1 ACREPORT                                                         
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         MVC   P(11),=C'** WRONG **'                                            
         EDIT  RECCNT,(14,P+20),0,MINUS=YES                                     
         GOTO1 ACREPORT                                                         
         MVC   P(11),=C'** TOTAL **'                                            
         EDIT  TRECCNT,(14,P+20),0,MINUS=YES                                    
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
REPORTIT NTR1                                                                   
         L     R5,AIO2                                                          
         USING TBARECD,R5                                                       
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         GOTO1 HEXOUT,DMCB,TBAKCPY,PCMPNY,1                                     
         EDIT  TBAKBTYP,(2,PTYPE)                                               
         MVC   PBREF,TBAKBREF                                                   
         EDIT  TBAKTSEQ,(3,PSEQ)                                                
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R3),DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP SOME RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
DMPGDIR  NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GETD'                                                      
         LA    RF,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DMPX                                                             
*                                                                               
DMPPDIR  NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'PUTD'                                                      
         LA    RF,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DMPX                                                             
*                                                                               
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET '                                                      
         B     DUMP                                                             
         SPACE 1                                                                
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT '                                                      
         B     DUMP                                                             
         SPACE 1                                                                
DUMP     L     R3,AIO2                                                          
         SR    RF,RF                                                            
         ICM   RF,3,TIMRLEN-TIMRECD(R3)                                         
DMPX     CLI   QOPT2,C'D'                                                       
         BNE   XIT                                                              
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R3),C'DUMP',(RF),=C'2D'                    
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
         GOTO1 HELLO,DMCB,(C'G',ACCMST),((R4),(R2)),((R5),(R3))                 
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
         GOTO1 HELLO,DMCB,(C'D',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3)                               
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
*        LITERALS                                                               
*****************************************                                       
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
         EJECT                                                                  
*****************************************                                       
*        EQUATES                                                                
*****************************************                                       
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   0                   END OF TABLE                                 
*                                                                               
TRECCNT  DC    PL8'0'                                                           
RECCNT   DC    PL8'0'                                                           
DRTOT    DC    PL8'0'                                                           
CRTOT    DC    PL8'0'                                                           
ITEMAMNT DC    PL6'0'                                                           
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'2000'                                                        
*                                                                               
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
DATVAL   DC    V(DATVAL)                                                        
HEXIN    DC    V(HEXIN)                                                         
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIO3     DC    A(IO3)                                                           
*                                                                               
DKEY     DS    CL(L'ACCKEY)                                                     
DKEYSV   DS    CL(L'ACCKEY)                                                     
KEYSV    DS    CL(L'ACCKEY)                                                     
DIR      DS    CL64                                                             
DA       DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IO1      DS    CL2000                                                           
IO2      DS    CL2000                                                           
IO3      DS    CL2000                                                           
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PCMPNY   DS    CL2                 COMPANY CODE                                 
         DS    CL3                                                              
PTYPE    DS    CL2                 BEFORE AFTER INDICATOR                       
         DS    CL4                                                              
PBREF    DS    CL4                 BAT REFERENCE                                
         DS    CL2                                                              
PSEQ     DS    CL3                 SEQ NUMBER                                   
         DS    CL2                                                              
PLNQ     EQU   *-PLINE                                                          
*                                                                               
*                                                                               
ACXUD    DSECT                                                                  
ELCODE   DS    XL1                                                              
ELM      DS    CL255                                                            
REPORTSW DS    CL1                                                              
HIT      DS    XL1                                                              
HITBAD   EQU   X'80'                                                            
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENMODES                                                                   
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074ACREPXU02 08/16/00'                                      
         END                                                                    
