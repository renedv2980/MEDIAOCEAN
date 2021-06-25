*          DATA SET SPINF47    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T21A47A                                                                  
         TITLE 'SPINFO47  -  STATION REP INFO - T21A47'                         
T21A47   CSECT                                                                  
LINLEN   EQU   88                                                               
         NMOD1 0,T21A47                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING REPRECD,R8                                                       
         SPACE                                                                  
* FILTER ROUTINE *                                                              
         SPACE                                                                  
FLT00    GOTO1 USER1,DMCB,(64,SINIFLT),(4,=C'SYND')                             
         OC    4(4,R1),4(R1)                                                    
         BZ    FMT00                                                            
         BAS   RF,HDRTN                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY                                                     
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH                           
         BZ    *+10                YES                                          
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HISTA                                                         
FLT10    CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   REPKAGY,AGYALPHA                                                 
         BNE   FLT20                                                            
         CLI   RSYND,C'S'                                                       
         BNE   FLT20                                                            
         MVC   8(3,R2),REPKREP                                                  
         MVC   13(65,R2),RNAME                                                  
         MVC   80(8,R2),R3LINE                                                  
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),LINLEN                                                     
         BNE   PAGEPRC                                                          
FLT20    BAS   RE,SEQSTA                                                        
         B     FLT10                                                            
         SPACE                                                                  
* DEFAULT FORMAT ROUTINE *                                                      
         SPACE                                                                  
FMT00    CLC   SINIFLT,SPACES      TEST FOR GARBAGE                             
         BE    FMT10               IN INPUT AREA                                
         OC    SINIFLT,SINIFLT                                                  
         BZ    FMT10                                                            
         LA    R2,SINIFLTH                                                      
         MVI   ERRCD,INVERR                                                     
         GOTO1 ERROR                                                            
FMT10    BAS   RF,HDRTN                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY                                                     
         OC    PREVKEY,PREVKEY     FIRST TIME THROUGH                           
         BZ    FMT15               YES                                          
         MVC   KEY,PREVKEY                                                      
FMT15    XC    PREVKEY,PREVKEY                                                  
         BAS   RE,HISTA                                                         
FMT20    CLC   REC(2),SVKEY                                                     
         BNE   EXIT                                                             
         CLC   REPKAGY,AGYALPHA                                                 
         BNE   FMT30                                                            
         MVC   8(3,R2),REPKREP                                                  
         MVC   13(65,R2),RNAME                                                  
         MVC   79(3,R2),R3LINE                                                  
         MVC   82(5,R2),RBIGZIP                                                 
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),LINLEN                                                     
         BNE   PAGEPRC                                                          
FMT30    BAS   RE,SEQSTA                                                        
         B     FMT20                                                            
*                                                                               
HDRTN    LA    R2,SINHDRH                                                       
         MVC   8(24,R2),=C'   NUMB / NAME / ADDRESS'                            
         FOUT  (R2)                FOUT HEADER                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   10(24,R2),=24C'-'                                                
         FOUT  (R2)                FOUT UNDERLINING                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RF                                                               
*                                                                               
HISTA    NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'STATI',KEY,AREC                      
         XIT1                                                                   
*                                                                               
SEQSTA   NTR1                                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMRSEQ',=C'STATI',KEY,AREC                      
         XIT1                                                                   
*                                                                               
PAGEPRC  MVC   PREVKEY,REC                                                      
         LA    R2,SINENDH                                                       
         OI    6(R2),X'C0'                                                      
         XIT1                                                                   
*                                                                               
EXIT     XC    PREVKEY,PREVKEY                                                  
         LA    R2,SINIKEYH                                                      
         OI    6(R2),X'C0'                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPINF47   05/01/02'                                      
         END                                                                    
