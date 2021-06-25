*          DATA SET ACREPXY02  AT LEVEL 042 AS OF 12/17/97                      
*PHASE ACXY02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'USE BATCH ITEM RECS TO FIND TRANS TO FIX'                       
ACXY02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXY**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXYD,RC                                                         
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
         MVC   TBAKCPY,RCCOMPFL                                                 
*                                                                               
         BAS   RE,HIGH                                                          
         B     *+8                                                              
RS00     BAS   RE,SEQ                                                           
         MVI   HIT,0                                                            
*                                                                               
         LA    R5,DIR                                                           
         CLI   TBARECD,TBAKTYPQ    HAS TO BE TRANS BATCH RECORD                 
         BNE   RECAP               I WON'T BE BACK                              
*                                                                               
         CLC   TBAKCPY,RCCOMPFL    MATCHING COMPANY?                            
         BNE   RECAP                                                            
*                                                                               
         OC    TBAKTSEQ,TBAKTSEQ   SKIP HEADER                                  
         BZ    RS00                                                             
*        CLI   TBAKBTYP,58                                                      
*        BNE   RS00                                                             
*        CLC   TBAKBMOS,=X'9912'                                                
*        BNH   RS00                                                             
         CLI   TBAKBTYP,26                                                      
         BNE   RS00                                                             
         CLC   TBAKADDT,=X'3C9E'                                                
         BH    RS00                                                             
*                                                                               
*        MVC   DKEYSV,DIR          SAVE KEY FOR DIRECTORY RESET                 
         L     R3,AIO2                                                          
         BAS   RE,GET                                                           
*        BAS   RE,DMPGET                                                        
         L     R5,AIO2                                                          
         USING BIAELD,R2                                                        
         LA    R2,TBARFST                                                       
RS02     CLI   0(R2),0                                                          
         BE    RS07                                                             
         CLI   0(R2),BIAELQ                                                     
         BNE   RS04                                                             
         ZAP   ITEMAMNT,BIAAMT                                                  
         B     RS06                                                             
RS04     CLI   0(R2),ASKELQ                                                     
         BE    RS08                                                             
RS06     SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     RS02                                                             
*                                                                               
RS07     TM    HIT,HITSE+HIT13     MISSING ONE OF THESE                         
         BNM   RS00                                                             
         AP    RECCNT,=P'1'                                                     
         AP    TRECCNT,=P'1'                                                    
         BAS   RE,DMPGET                                                        
         B     RS00                                                             
*                                                                               
         USING ASKELD,R2                                                        
         USING TRNRECD,R5                                                       
RS08     DS    0H                                                               
         CLC   ASKKEY+1(2),=C'SE'                                               
         BNE   RS10                                                             
         OI    HIT,HITSE                                                        
         B     RS06                                                             
RS10     CLC   ASKKEY+1(2),=C'13'                                               
         BNE   RS06                                                             
         OI    HIT,HIT13                                                        
         B     RS06                                                             
*                                                                               
*&&UK                                                                           
RS08     MVC   DKEY,ASKKEY                                                      
         LA    R5,DIR                                                           
         BAS   RE,READ                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         BAS   RE,DMPGDIR          DUMP DIR RECORD                              
         MVC   TRNKSMOS,=X'9707'                                                
         L     R3,AIO2                                                          
         BAS   RE,GET                                                           
         BAS   RE,DMPGET                                                        
         MVI   REPORTSW,C'B'                                                    
         CLI   QOPT1,C'B'          REPORT BEFORE AND AFTER                      
         BNE   *+8                                                              
         BAS   RE,REPORTIT                                                      
         L     R5,AIO2                                                          
         USING TRNRECD,R5                                                       
         CLI   TRNRSTYP,X'3A'                 TYPE 58                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TRNRSMOS,=X'9707'                                                
         LA    R2,TRNRFST                                                       
RS12     CLI   0(R2),0                                                          
         BE    RS20                                                             
         USING TRNELD,R2                                                        
         CLI   0(R2),X'44'                                                      
         BNE   RS14                                                             
         CP    ITEMAMNT,TRNAMNT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,DRTOT            ADD TO TOTALS                                
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,CRTOT                                                         
         AP    0(L'DRTOT,R1),TRNAMNT                                            
         MVC   TRNMOS,=C'77'                                                    
         B     RS16                                                             
*                                                                               
         USING TRSELD,R2                                                        
RS14     CLI   0(R2),X'60'                                                      
         BNE   RS16                                                             
         MVC   TRSPMOS,=X'9707'                                                 
*                                                                               
RS16     SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     RS12                                                             
*                                                                               
RS20     DS    0H                                                               
         BAS   RE,WRT              WRITE TRANSACTION DIR BACK                   
         BAS   RE,DMPPDIR                                                       
         L     R3,AIO2                                                          
         BAS   RE,PUT              WRITE TRANSACTION BACK                       
         BAS   RE,DMPPUT                                                        
         MVI   REPORTSW,C'A'                                                    
         BAS   RE,REPORTIT                                                      
         MVC   DKEY,DKEYSV         RESET DIRECTORY FOR BATCH RECORDS            
         BAS   RE,HIGH                                                          
         B     RS00                                                             
*&&                                                                             
         EJECT                                                                  
RUNL     CLI   MODE,RUNFRST                                                     
         BNE   XIT                                                              
         GOTO1 ACREPORT                                                         
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         MVC   P(11),=C'** TOTAL **'                                            
         EDIT  TRECCNT,(14,PCOUNT),2,MINUS=YES                                  
         B     XIT                                                              
RECAP    DS    0H                                                               
         GOTO1 ACREPORT                                                         
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         MVC   P(11),=C'** TOTAL **'                                            
         EDIT  RECCNT,(14,PCOUNT),2,MINUS=YES                                   
*        EDIT  DRTOT,(14,PDEBITS),2,MINUS=YES                                   
         EDIT  CRTOT,(14,PCREDITS),2,MINUS=YES                                  
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
REPORTIT NTR1                                                                   
         L     R5,AIO2                                                          
         USING TRNRECD,R5                                                       
         LA    R6,P                                                             
         USING PLINE,R6                                                         
         GOTO1 HEXOUT,DMCB,TRNKCPY,PCMPNY,1                                     
         MVC   PIND,REPORTSW                                                    
         MVC   PACCOUNT,TRNKULA                                                 
         MVC   PCONTRA,TRNKULC                                                  
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(5,PDATE)                               
         MVC   PREF,TRNKREF                                                     
         MVC   WORK(2),TRNRSMOS                                                 
         MVC   WORK+2(1),=X'01'                                                 
         GOTO1 DATCON,DMCB,(1,WORK),(9,PMOA)                                    
         EDIT  TRNKSBR,(3,PSEQ)                                                 
         LA    R2,TRNRFST                                                       
         CLI   0(R2),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,R2                                                        
         MVC   PBREF,TRNBTCH                                                    
         LA    R3,PDEBITS                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R3,PCREDITS                                                      
         EDIT  (P6,TRNAMNT),(14,(R3)),2,MINUS=YES                               
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
PIND     DS    CL1                 BEFORE AFTER INDICATOR                       
         DS    CL2                                                              
PACCOUNT DS    CL14                ACCOUNT                                      
         DS    CL2                                                              
PCONTRA  DS    CL14                CONTRA                                       
         DS    CL2                                                              
PDATE    DS    CL8                 DATE                                         
         DS    CL2                                                              
PREF     DS    CL6                 REFERENCE                                    
         DS    CL2                                                              
PBREF    DS    CL6                 BAT REFERENCE                                
         DS    CL2                                                              
PSEQ     DS    CL3                 SEQ NUMBER                                   
         DS    CL2                                                              
PMOA     DS    CL6                 MOA                                          
         DS    CL2                                                              
PDEBITS  DS    CL14                DR                                           
         ORG   PDEBITS                                                          
PCOUNT   DS    CL14                RECORD COUNT                                 
         DS    CL2                                                              
PCREDITS DS    CL14                CR                                           
PLNQ     EQU   *-PLINE                                                          
*                                                                               
*                                                                               
ACXYD    DSECT                                                                  
ELCODE   DS    XL1                                                              
ELM      DS    CL255                                                            
REPORTSW DS    CL1                                                              
HIT      DS    XL1                                                              
HITSE    EQU   X'80'                                                            
HIT13    EQU   X'40'                                                            
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
**PAN#1  DC    CL21'042ACREPXY02 12/17/97'                                      
         END                                                                    
