*          DATA SET SPLFM23    AT LEVEL 033 AS OF 11/24/92                      
*PHASE T21923A,+0                                                               
         TITLE 'SPLFM23A - REP RECORD  T21923A'                                 
T21923   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21923                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING REPRECD,R8                                                       
         CLI   SVFMTSW,0                     TEST FORMAT OR EDIT                
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
         FOUT  REPNAMEH,RNAME,22                                                
         FOUT  REPSTADH,R1LINE,24                                               
         FOUT  REPCITYH,R2LINE,24                                               
         FOUT  REPSTATH,R3LINE,3                                                
         LA    R2,REPCOUNH                                                      
         MVI   8(R2),C'U'                                                       
         CLC   RZIP,=C'CANAD'                                                   
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
         OI    6(R2),X'80'                                                      
         FOUT  REPBZIPH,RBIGZIP,10                                              
         LA    R2,REPNTWKH                                                      
         MVI   8(R2),C'N'                                                       
         CLI   RUNWNET,C'Y'                                                     
         BNE   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'                                                      
         B     EXXMOD                                                           
         EJECT                                                                  
EDT      CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
EDT0     LA    R2,REPNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   RNAME,8(R2)                                                      
         LA    R2,REPSTADH                                                      
         GOTO1 ANY                                                              
         MVC   R1LINE,8(R2)                                                     
         LA    R2,REPCITYH                                                      
         GOTO1 ANY                                                              
         MVC   R2LINE,8(R2)                                                     
*                                                                               
*                                  STATE CODE NOW AFTER ZIP LOGIC               
EDT4     XC    RZIP,RZIP                                                        
         LA    R2,REPCOUNH                                                      
         CLI   8(R2),C'C'                                                       
         BNE   EDT4D                                                            
         MVC   RZIP,=C'CANAD'                                                   
*                                                                               
EDT4D    LA    R2,REPSTATH                                                      
         MVI   ERRCD,INVERR                                                     
         TM    4(R2),X'04'          ALPHA                                       
         BZ    LFMERR                                                           
         CLC   RZIP,=C'CANAD'      SEE IF CANADIAN STATION                      
         BE    EDT4F               ACCEPT 2 OR 3 CHARS                          
         CLI   5(R2),2                                                          
         BNE   LFMERR                                                           
         B     EDT4X                                                            
*                                                                               
EDT4F    CLI   5(R2),2                                                          
         BL    LFMERR                                                           
         CLI   5(R2),3                                                          
         BH    LFMERR                                                           
EDT4X    MVC   R3LINE(3),8(R2)                                                  
         OC    R3LINE,SPACES                                                    
*                                                                               
EDT5     LA    R2,REPBZIPH                                                      
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    LFMERR                                                           
         MVC   RBIGZIP,8(R2)                                                    
*                                                                               
EDT6     LA    R2,REPNTWKH                                                      
         MVI   RUNWNET,C'N'                                                     
         CLI   8(R2),C'Y'                                                       
         BNE   *+10                                                             
         MVC   RUNWNET,8(R2)                                                    
*                                                                               
EDTX     ST    R8,AREC             RESET AREC TO                                
         MVC   KEY,SVKEY                                                        
         CLI   SVACT,C'A'          TEST ADD                                     
         BNE   WRITE                                                            
         MVC   REC(17),SVKEY                                                    
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
*            KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            REP-TYPE RECORDS IS TO BE 144.                                     
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
         MVC   COMMAND,=C'DMADD'                                                
         GOTO1 STA                                                              
         GOTO1 CNADDSTA                                                         
         B     REQREC                                                           
*                                                                               
WRITE    LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 RDSTA               REREAD REC BEFORE WRITE                      
         TM    DMCB+8,X'50'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
* 10/22/92   KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            REP-TYPE RECORDS IS TO BE 144.                                     
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
         ST    R8,AREC                                                          
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 STA                                                              
         GOTO1 CNCHASTA                                                         
         B     REQREC                                                           
         EJECT                                                                  
REQREC   XC    REC(110),REC             GENERATE REQUEST RECORD                 
         MVI   REC+10,45                                                        
         MVI   REC+14,106                                                       
         MVI   REC+26,X'40'                                                     
         MVC   REC+27(79),REC+26                                                
         MVC   REC+26(2),=C'45'                                                 
         MVC   REC+28(2),AGYALPHA                                               
         MVC   REC+30(1),SVEBCMED                                               
         MVC   REC+31(3),=C'ALL'                                                
         MVC   REC+36(1),SVKEY                                                  
         MVC   REC+44(5),SVKEY+2                                                
         MVC   REC+94(7),=C'CONTROL'                                            
         MVC   REC+93(1),SVACT                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                      
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXXMOD                                                           
*                                                                               
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFME3                                                                        
       ++INCLUDE SPLFME3D                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPLFM23   11/24/92'                                      
         END                                                                    
