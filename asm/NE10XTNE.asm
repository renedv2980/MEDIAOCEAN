*          DATA SET NE10XTNE   AT LEVEL 012 AS OF 05/01/02                      
*PHASE PZNEEXTA                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'PZNEXTNE - CHANGE NE TO RP '                                    
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
PZNEXTNE CSECT                                                                  
         NMOD1 40,DMLDUMP,RR=R2                                                 
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R2,RELO                                                          
         L     R3,=V(HEXOUT)                                                    
         AR    R3,R2                                                            
         ST    R3,VHEXOUT                                                       
*                                                                               
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
*        L     RF,CNTR             KEEP COUNT OF PURGES                         
*        LA    RF,1(RF)                                                         
*        ST    RF,CNTR                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         XC    CNTR,CNTR           CLEAR COUNTER                                
         XC    CNTR2,CNTR2         CLEAR COUNTER                                
         LA    R2,FILEOUT          OPEN TAPE                                    
         OPEN  ((R2),(OUTPUT))                                                  
         B     DMXIT                                                            
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               BUFNO=2,                                                X        
               MACRF=PM                                                         
         SPACE 2                                                                
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
         MVC   P(16),=C'PACKAGE RECORDS='                                       
         EDIT  (B4,CNTR),(7,P+17),ALIGN=LEFT                                    
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'UNIT RECORDS='                                          
         EDIT  (B4,CNTR2),(7,P+14),ALIGN=LEFT                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R2,FILEOUT          CLOSE TAPE                                   
         CLOSE ((R2),)                                                          
         B     DMXIT                                                            
*                                                                               
*                                                                               
CNTR     DS    F                                                                
CNTR2    DS    F                                                                
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING NPRECD,R3                                                        
         CLI   0(R3),X'02'         PACKAGE                                      
         BNE   DMXRECU                                                          
         CLI   NPKAM,X'F3'         TEST SJ3                                     
         BNE   DMXPURGE                                                         
         BAS   RE,PUTOUT                                                        
         L     R1,CNTR                                                          
         LA    R1,1(R1)                                                         
         ST    R1,CNTR                                                          
*        CLC   CNTR,=F'10'                                                      
*        BH    DMX5                                                             
*        GOTO1 VHEXOUT,DMCB,(R3),P,50                                           
*        GOTO1 VPRINTER                                                         
DMX5     B     DMXKEEP                                                          
*                                                                               
DMXRECU  CLC   0(2,R3),=X'04F3'        TEST SJR UNIT                            
         BNE   DMXPURGE                                                         
         L     R1,CNTR2                                                         
         LA    R1,1(R1)                                                         
         ST    R1,CNTR2                                                         
         BAS   RE,PUTOUT                                                        
*        CLC   CNTR2,=F'10'                                                     
*        BH    DMXU5                                                            
*        GOTO1 VHEXOUT,DMCB,(R3),P,50                                           
*        GOTO1 VPRINTER                                                         
DMXU5    B     DMXKEEP                                                          
*                                                                               
*                                                                               
PUTOUT   NTR1                                                                   
         LR    R5,R3                                                            
         SH    R5,=H'4'                                                         
         PUT   FILEOUT,(R5)                                                     
PTOUTX   XIT1                                                                   
         EJECT                                                                  
         SPACE                                                                  
         LTORG                                                                  
*                                                                               
WORKAREA DS    CL2004                                                           
*                                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
FULL     DS    F                                                                
RELO     DS    F                                                                
DUB      DS    D                                                                
PAKFLD   DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
WORK     DS    CL32                                                             
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
VHEXOUT  DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NE10XTNE  05/01/02'                                      
         END                                                                    
