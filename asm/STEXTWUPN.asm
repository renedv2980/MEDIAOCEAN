*          DATA SET STEXTWUPN  AT LEVEL 007 AS OF 11/01/96                      
*          DATA SET STEXTCC    AT LEVEL 038 AS OF 07/12/93                      
*PHASE STEXTWU,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'DMLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
*                                                                               
* MOVE  WUPN TO WPNY FOR ALL AGENCIES EXCEPT JWNY, WILA                         
* NOV03/96                                                                      
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(FILEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
**NOP**  B     DMXIT                                                            
         L     R3,AREC                                                          
         SR    R0,R0                                                            
         ICM   R0,3,15(R3)         GET LENGTH                                   
         AH    R0,=H'4'                                                         
         SLL   R0,16                                                            
         SH    R3,=H'4'                                                         
         ST    R0,0(R3)            SET LENGTH IN FRONT OF RECORD                
         PUT   FILEOUT,(3)                                                      
         B     DMXPURGE                                                         
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         AP    TOTRD,=P'1'                                                      
*                                                                               
         USING STARECD,R3                                                       
         CLI   STAKTYPE,C'S'       STATION REC?                                 
         BNE   DMXKEEP                                                          
         CLI   STAKMED,C'T'        MEDIA TV                                     
         BNE   DMXKEEP                                                          
         AP    TSTACT,=P'1'                                                     
         CLC   STAKAGY,=C'JW'      JWNY ALREADY DONE                            
         BE    DMXREC04                                                         
         CLC   STAKAGY,=C'WI'      WILA ALREADY DONE                            
         BE    DMXREC04                                                         
         CLC   STAKAGY,=C'WR'      WRLA ALREADY DONE                            
         BE    DMXREC04                                                         
         CLC   STAKAGY,=C'WT'      WITO ALREADY DONE                            
         BNE   DMXREC06                                                         
DMXREC04 AP    BYPASS,=P'1'                                                     
         B     DMXKEEP                                                          
*                                                                               
DMXREC06 CLC   =C'WUPN',STAKCALL                                                
         BNE   DMXREC10                                                         
         SPACE                                                                  
         AP    TWUPN,=P'1'                                                      
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'STATION'                                              
         MVC   P+18(5),STAKCALL                                                 
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   STAKCALL(4),=C'WPNY'                                             
         B     DMXKEEP                                                          
*                                                                               
DMXREC10 CLC   =C'WGGT',STAKCALL                                                
         BNE   DMXREC20                                                         
         SPACE                                                                  
         AP    TWGGT,=P'1'                                                      
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'STATION'                                              
         MVC   P+18(5),STAKCALL                                                 
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   STAKCALL(4),=C'WUPN'                                             
         B     DMXKEEP                                                          
*                                                                               
DMXREC20 CLC   =C'WPNY',STAKCALL                                                
         BNE   DMXREC30                                                         
         SPACE                                                                  
         AP    TWPNY,=P'1'                                                      
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'STATION'                                              
         MVC   P+18(5),STAKCALL                                                 
         MVC   P+25(5),=C'*****'                                                
         B     DMXKEEP                                                          
*                                                                               
DMXREC30 CLC   =C'WUUU',STAKCALL                                                
         BNE   DMXKEEP                                                          
         SPACE                                                                  
         AP    TWUUU,=P'1'                                                      
         MVC   P(7),=C'STA REC'                                                 
         MVC   P+10(7),=C'STATION'                                              
         MVC   P+18(5),STAKCALL                                                 
         MVC   P+25(5),=C'*****'                                                
*                                                                               
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         DROP  R3                                                               
         EJECT                                                                  
PRT      DS    0H                                                               
         LA    R5,116                                                           
         SPACE                                                                  
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'0D'               
         B     DMXKEEP                                                          
         SPACE                                                                  
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         CLOSE FILEOUT                                                          
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(100),RTITLE                                                    
         GOTO1 VPRINTER                                                         
         LA    R2,TOTCTRS                                                       
         LA    R3,TOTRD                                                         
DMXEOF10 MVC   P+5(28),5(R3)                                                    
         EDIT  (P5,0(R3)),(8,P+33)                                              
         GOTO1 VPRINTER                                                         
         LA    R3,33(,R3)                                                       
         BCT   R2,DMXEOF10                                                      
         SPACE                                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE                                                                  
         B     DMXIT                                                            
         EJECT                                                                  
TOTRD    DC    PL5'0',CL28'TOT RECS READ'                                       
BYPASS   DC    PL5'0',CL28'AGYS BYPASSED'                                       
TSTACT   DC    PL5'0',CL28'TOT STA RECS'                                        
TWUPN    DC    PL5'0',CL28'TOT WUPN    '                                        
TWGGT    DC    PL5'0',CL28'TOT WGGT    '                                        
TWPNY    DC    PL5'0',CL28'TOT WPNY    '                                        
TWUUU    DC    PL5'0',CL28'TOT WUUU    '                                        
TOTCTRS  EQU   (*-TOTRD)/33                                                     
          SPACE                                                                 
WORK     DS    CL64                                                             
         SPACE                                                                  
RTITLE   DC    CL100'RADIO STATION WUPN BECOMES WPNY, WGGT - WUPN'              
         LTORG                                                                  
          SPACE                                                                 
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
          SPACE                                                                 
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VFILEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
MTABLED  DSECT                                                                  
MTABENT  DS   0XL16                                                             
MNEWMKT  DS    CL4                                                              
MOLDMKT  DS    CL4                                                              
MSTA     DS    CL5                                                              
MMKTCHG  DS    CL1                                                              
MSTACHG  DS    CL1                                                              
         DS    XL1                                                              
MTABNXT  EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
STARECD   DSECT                                                                 
       ++INCLUDE SPGENSTA                                                       
MKTRECD   DSECT                                                                 
       ++INCLUDE SPGENMKT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007STEXTWUPN 11/01/96'                                      
         END                                                                    
