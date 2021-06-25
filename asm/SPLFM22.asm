*          DATA SET SPLFM22    AT LEVEL 034 AS OF 10/01/96                      
*PHASE T21922A,+0                                                               
         TITLE 'SPLFM22 - STATION ADDRESS  T21922'                              
T21922   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21922                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING ADRRECD,R8                                                       
*                                                                               
         L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3                                                        
         BE    MAIN10                                                           
         DROP  R1,RF                                                            
*                                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(37),=C'** ERROR ** USE SPECIAL FILE MAINT **'             
         MVI   ERRAREA,X'FF'                                                    
         FOUT  LFMMSGH                                                          
         OI    6(R2),X'40'                                                      
         B     LFMERR                                                           
*~                                                                              
MAIN10   CLI   SVFMTSW,0                     TEST FORMAT OR EDIT                
         BE    FMT                                                              
         B     EDT                                                              
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
         FOUT  ADRNAMEH,ANAME,20                                                
         FOUT  ADRSTADH,A1LINE,24                                               
         FOUT  ADRCITYH,A2LINE,24                                               
         FOUT  ADRSTATH,A3LINE,3                                                
         LA    R2,ADRCOUNH                                                      
         MVI   8(R2),C'U'                                                       
         CLC   AZIP,=C'CANAD'                                                   
         BNE   *+8                                                              
         MVI   8(R2),C'C'                                                       
         OI    6(R2),X'80'                                                      
         FOUT  ADRBZIPH,ABIGZIP,10                                              
         B     EXXMOD                                                           
         EJECT                                                                  
EDT      CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 RDSTA                                                            
EDT0     LA    R2,ADRNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   ANAME,8(R2)                                                      
         LA    R2,ADRSTADH                                                      
         GOTO1 ANY                                                              
         MVC   A1LINE,8(R2)                                                     
         LA    R2,ADRCITYH                                                      
         GOTO1 ANY                                                              
         MVC   A2LINE,8(R2)                                                     
*                                  STATE CODE NOW AFTER ZIP LOGIC               
*                                                                               
EDT4     XC    AZIP,AZIP                                                        
         LA    R2,ADRCOUNH                                                      
         CLI   8(R2),C'C'                                                       
         BNE   EDT4D                                                            
         MVC   AZIP,=C'CANAD'                                                   
*                                                                               
EDT4D    LA    R2,ADRSTATH                                                      
         MVI   ERRCD,INVERR                                                     
         TM    4(R2),X'04'          ALPHA                                       
         BZ    LFMERR                                                           
         CLC   AZIP,=C'CANAD'      SEE IF CANADIAN STATION                      
         BE    EDT4F               ACCEPT 2 OR 3 CHARS                          
         CLI   5(R2),2                                                          
         BNE   LFMERR                                                           
         B     EDT4X                                                            
*                                                                               
EDT4F    CLI   5(R2),2                                                          
         BL    LFMERR                                                           
         CLI   5(R2),3                                                          
         BH    LFMERR                                                           
EDT4X    MVC   A3LINE(3),8(R2)                                                  
         OC    A3LINE,SPACES                                                    
         B     EDT5                                                             
EDT5     LA    R2,ADRBZIPH                                                      
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    LFMERR                                                           
         MVC   ABIGZIP,8(R2)                                                    
*                                                                               
EDTX     ST    R8,AREC             RESET AREC-->REC                             
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
*            'A'-TYPE RECORDS IS TO BE 144.                                     
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
         ST    R8,AREC             R8-->REC.                                    
*                                                                               
***********************************************************************         
*ATTENTION:  THE BELOW MVC INSTRUCTION IS INSERTED FOR A FILE-FIX ON            
*            THE STATION FILES.  THEY ARE BEING CONVERTED FROM 17-BYTES         
*            KEY, FIXED-LENGTH RECORDS TO 15-BYTES KEY AND VARIABLE-            
*            LENGTH RECORDS.  THE LENGTH OF THE NEW RECORDS IS PLACED           
*            IN THE 16TH AND 17TH BYTES, AND THE LENGTH OF THE                  
*            'A'-TYPE RECORDS IS TO BE 144.                                     
*                                                                               
         MVC   REC+15(2),=H'144'                                                
*                                                                               
***********************************************************************         
*                                                                               
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
         CLC   SVKEY+9(3),=C'000'                                               
         BE    *+10                                                             
         MVC   REC+31(3),SVKEY+9                                                
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
*SPLFME2                                                                        
       ++INCLUDE SPLFME2D                                                       
ADRRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
         SPACE 2                                                                
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SPLFM22   10/01/96'                                      
         END                                                                    
