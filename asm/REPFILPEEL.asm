*          DATA SET REPFILPEEL AT LEVEL 003 AS OF 08/31/00                      
*          DATA SET REPFILPEEL AT LEVEL 002 AS OF 12/17/87                      
*PHASE REFP02A                                                                  
         TITLE 'REP FILE PEEL - REFP02'                                         
REFP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REFP**,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         SPACE 4                                                                
         CLI   MODE,REQFRST                                                     
         BE    FP10                                                             
FPXIT    XIT1                                                                   
         EJECT                                                                  
FP10     DS    0H                                                               
         OPEN  (FILEIN,(INPUT))                                                 
         OPEN  (FILOUTA,(OUTPUT))                                               
         SPACE 1                                                                
         CLC   QASAT,SPACES                                                     
         BE    FP50                                                             
         MVC   DUB(6),QASAT        CAN OVERRIDE TODAY                           
         B     FP100                                                            
FP50     GOTO1 DATCON,DMCB,(5,0),DUB      TODAY'S DATE                          
FP100    GOTO1 GETDAY,DMCB,DUB,FULL       DAY OF WEEK                           
         SR    R3,R3                                                            
         IC    R3,DMCB                                                          
         BCTR  R3,0                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,DUB,DMCB+12,(R3)                                      
         GOTO1 DATCON,DMCB,DMCB+12,(3,MDATE)                                    
         SPACE 1                                                                
*                                  TO ENSURE AT LEAST ONE RECORD                
         EJECT                                                                  
*    READ IN RECORDS FROM REP FILE TAPE AND PROCESS THEM                        
FP200    DS    0H'0'                                                            
*        AP    RECCNT,=P'1'        ***TEST-READ ONLY 500 RECORDS***             
*        CP    RECCNT,=P'500'      ***TEST***                                   
*        BH    FPEOF               ***TEST***                                   
         LA    R0,REC-4            POINT TO REC-4                               
         GET   FILEIN,(R0)         (ALLOW FOR 4-BYTE HEADER)                    
         SPACE 1                                                                
*  SET 2X'00' AT EOR (FOR GETEL)                                                
         SPACE 1                                                                
         LA    RE,REC-4                                                         
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)       PUT ZERO AT END OF RECORD                    
         SPACE 1                                                                
         LA    R3,REC                                                           
         CLI   0(R3),X'02'         STATION                                      
         BNE   FP210                                                            
         CLC   20(2,R3),=C'SJ'                                                  
         BNE   FP200                                                            
         B     KEEP                                                             
         SPACE 1                                                                
FP210    CLI   0(R3),X'08'         ADVERTISER                                   
         BNE   FP220                                                            
         CLC   25(2,R3),=C'SJ'                                                  
         BNE   FP200                                                            
         B     KEEP                                                             
         SPACE 1                                                                
FP220    CLI   0(R3),X'0C'         CONTRACT                                     
         BNE   FP230                                                            
         CLC   2(2,R3),=C'SJ'                                                   
         BNE   FP200                                                            
         B     KEEP                                                             
         SPACE 1                                                                
FP230    CLI   0(R3),X'27'         ATHENA                                       
         BNE   FP200                                                            
         CLC   1(2,R3),=C'SJ'                                                   
         BNE   FP200                                                            
         B     KEEP                                                             
         SPACE 2                                                                
KEEP     LA    R0,REC-4                                                         
         PUT   FILOUTA,(R0)        PUT RECORD TO OUTPUT                         
         B     FP200               CONTINUE                                     
         SPACE 2                                                                
FPEOF    DS    0H'0'                                                            
         CLOSE FILEIN                                                           
         CLOSE FILOUTA                                                          
         B     FPXIT                                                            
         EJECT                                                                  
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
MDATE    DS    CL3   (BINARY YMD MONDAY OF THIS WEEK (OR MON OF ASAT)           
         SPACE 3                                                                
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=FPEOF                                                      
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BLKSIZE=32760,BUFNO=2                                 
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         DS    F                   LENGTH OF RECORD                             
REC      DS    CL1008              AREA FOR RECORD                              
         SPACE 1                                                                
         EJECT                                                                  
SWID     DSECT                                                                  
       ++INCLUDE REGENSWI                                                       
         EJECT                                                                  
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
         PRINT OFF                                                              
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003REPFILPEEL08/31/00'                                      
         END                                                                    
