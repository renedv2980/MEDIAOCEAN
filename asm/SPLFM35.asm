*          DATA SET SPLFM35    AT LEVEL 008 AS OF 08/11/00                      
*PHASE T21935A                                                                  
         TITLE 'T21935 - EQUIVALENCY HEADER'                                    
T21935   CSECT                                                                  
         NMOD1 0,T21935                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING EQUHDRD,R8                                                       
         CLI   SVFMTSW,0                                                        
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
*                                                                               
FMT0     LA    R4,EQUSECT1                                                      
         LA    R2,EQUS10H                                                       
*                                                                               
FMT1     EDIT  (2,0(R4)),(4,FULL),0,ZERO=NOBLANK,ALIGN=LEFT                     
         FOUT  (R2),FULL,4                                                      
         OI    4(R2),II1C                                                       
*                                                                               
         ZIC   R3,0(R2)           ADVANCE TO NEXT OUTPUT SLOT                   
         AR    R2,R3                                                            
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         LA    R0,EQUTABH                                                       
         CR    R2,R0                                                            
         BNL   FMT2                                                             
         LA    R4,4(R4)           ADVANCE NEXT EQUHDR SLOT                      
         B     FMT1                                                             
*                                                                               
FMT2     B     EXXMOD                                                           
         EJECT                                                                  
EDT      DS    0H                                                               
         MVI   BYTE2,0                                                          
         CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
EDT0     MVC   EQULEN,=AL2(162)                                                 
         MVC   EQUEL(2),=X'098A'                                                
         LA    R4,EQUSECT1                                                      
         LA    R2,EQUS10H                                                       
*                                                                               
EDT1     CLI   SVACT,C'A'                                                       
         BE    EDT2                                                             
         TM    4(R2),II1C                                                       
         BZ    EDT2                                                             
EDT1A    CLC   8(4,R2),=C'1000'    IF VAL=1000 SET SWITCH                       
         BNE   EDT1B                                                            
         OI    BYTE2,X'80'                                                      
EDT1B    ZIC   R3,0(R2)            ADVANCE TO NEXT INPUT SLOT                   
         AR    R2,R3                                                            
         ZIC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         LA    R0,EQUTABH                                                       
         CR    R2,R0                                                            
         BNL   EDT3                                                             
         LA    R4,4(R4)           ADVANCE EQUHDR SLOT                           
         B     EDT1                                                             
*                                                                               
EDT2     MVI   ERRCD,NOTNUM                                                     
         GOTO1 PACK                                                             
         LTR   R0,R0                                                            
         BZ    LFMERR                                                           
         STH   R0,HALF                                                          
         MVC   0(2,R4),HALF       REPLACE FOR SOLO                              
         MVC   2(2,R4),HALF         AND PIGGYBACK                               
         B     EDT1A                                                            
*                                                                               
EDT3     MVI   ERRCD,NO1000                                                     
         LA    R2,EQUS10H         SET CURSOR TO FIRST SLOT                      
         TM    BYTE2,X'80'        NO VALUES OF 1000 PRESENT                     
         BZ    LFMERR                                                           
         MVC   EQUSECT2,EQUSECT1  REPLACE SECOND SET OF EQUIVS TOO              
         MVC   EQUDPT(2),=X'0001' SET UP DAYPART POINTERS                       
         MVC   EQUDPT+2(14),EQUDPT+1                                            
         EJECT                                                                  
OUTPUT   MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         CLI   SVACT,C'A'                                                       
         BNE   OUT1                                                             
         MVC   REC(13),SVKEY                                                    
*                                                                               
         GOTO1 ADDREC                                                           
         B     FMT0                                                             
*                                                                               
OUT1     GOTO1 PUTREC                                                           
         B     FMT0                                                             
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMD5D                                                       
         EJECT                                                                  
EQUHDRD  DSECT                                                                  
       ++INCLUDE SPGENEQU                                                       
         SPACE 4                                                                
NO1000   EQU   170                ERRMSG - NO 1000 EQU VALUES PRESENT           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPLFM35   08/11/00'                                      
         END                                                                    
