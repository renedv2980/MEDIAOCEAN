*          DATA SET REUPOVR    AT LEVEL 031 AS OF 08/31/00                      
*          DATA SET REUPOVR    AT LEVEL 030 AS OF 12/14/90                      
*PHASE UPOVRA                                                                   
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'BLAIR OVERNITES UPLOAD'                                         
UPOVR    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**UPOVR*,=V(REGSAVE)                                           
*        GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         EJECT                                                                  
         OPEN  (FILE,OUTPUT)                                                    
         SPACE 2                                                                
         LA    R8,REC                                                           
         USING ROV,R8                                                           
         LA    R0,901                                                           
         STH   R0,REC                                                           
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         XC    SCREEN,SCREEN                                                    
         GOTO1 =V(DATCON),DMCB,(5,0),(3,TODAY)                                  
         SPACE 1                                                                
OVR3     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD                                                      
         BE    XIT                                                              
         SPACE 1                                                                
         LA    R4,REPTBL           REPORT TABLE                                 
         LA    R5,SVCTBL           SERVICE TABLE                                
         LA    R6,120              SCREEN COUNTER                               
******** LA    R6,720              TEMPORARY                                    
         SPACE 1                                                                
OVR5     MVI   ROVRKTYP,X'22'      RECORD TYPE                                  
         MVC   ROVRKREP,CARD       REP                                          
         MVC   ROVRKRPT,0(R4)      REPORT                                       
         MVC   ROVRKSVC,0(R5)      SERVICE                                      
         MVC   ROVRKMKT,CARD+2     MARKET                                       
         LH    R7,SCREEN                                                        
         LA    R7,1(R7)                                                         
         STH   R7,SCREEN                                                        
         CVD   R7,DUB                                                           
         UNPK  ROVRKSCN,DUB                                                     
         OI    ROVRKSCN+3,X'F0'    SCREEN                                       
         MVC   ROVRLEN,=X'0381'    RECORD LENGTH                                
         SPACE 1                                                                
         MVC   ROVRCODE(2),=X'0114'    BUILD X'01' ELEMENT                      
         SPACE 1                                                                
         LA    R7,58(R8)           BUILD 6 TEXT (X'02') ELEMENTS                
         USING ROVRTXEL,R7                                                      
         MVI   BYTE,1                                                           
         SR    RE,RE                                                            
OVR10    MVC   ROVRTXCD(2),=X'0289'                                             
         MVC   ROVRTXSQ,BYTE       SEQUENCE NUMBER                              
         MVC   ROVRTXL,SPACES                                                   
         MVC   ROVRTXR,SPACES                                                   
         LA    R7,137(R7)                                                       
         IC    RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         CLI   BYTE,7                                                           
         BL    OVR10                                                            
         DROP  R7                                                               
         SPACE 1                                                                
         MVC   0(2,R7),=X'F114'    BUILD ACTIVITY (X'F1') ELEMENT               
         MVC   2(3,R7),TODAY                                                    
         MVC   5(2,R7),=X'0050'    HARD CODE FOR BLAIR                          
*******  MVC   5(2,R7),=X'0650'    HARD CODE FOR NBC                            
*******  MVC   5(2,R7),=X'05AC'    HARD CODE FOR TELEMUNDO                      
*******  MVC   5(2,R7),=X'078C'    HARD CODE FOR UNIVISION                      
         MVC   8(3,R7),TODAY                                                    
         MVC   11(2,R7),=X'0050'   HARD CODE FOR BLAIR                          
*******  MVC   11(2,R7),=X'0650'   HARD CODE FOR NBC                            
*******  MVC   11(2,R7),=X'05AC'   HARD CODE FOR TELEMUNDO                      
*******  MVC   11(2,R7),=X'078C'   HARD CODE FOR UNIVISION                      
         SPACE 1                                                                
         PUT   FILE,(R8)                                                        
         SPACE 1                                                                
         BCT   R6,OVR5             CREATE 120 SCREENS                           
         XC    SCREEN,SCREEN                                                    
         SPACE 1                                                                
         LA    R5,L'SVCTBL(R5)     NEXT SERVICE                                 
         CLI   0(R5),X'FF'                                                      
         BE    OVR20                                                            
         LA    R6,120                                                           
******** LA    R6,720              **TEMP                                       
         B     OVR5                                                             
         SPACE 1                                                                
OVR20    LA    R4,L'REPTBL(R4)     NEXT REPORT TYPE                             
         CLI   0(R4),X'FF'                                                      
         BE    OVR3                                                             
         LA    R5,SVCTBL                                                        
         LA    R6,120                                                           
******** LA    R6,720              ** TEMP                                      
         B     OVR5                                                             
         SPACE 1                                                                
XIT      CLOSE (FILE,)                                                          
         XBASE                                                                  
         EJECT                                                                  
DMCB     DS    6F                                                               
DUMPLIST DS    0F                                                               
         DC    A(UPOVR,25000)                                                   
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
WORK     DS    CL64                                                             
DUB      DS    D                                                                
CARD     DS    CL80                                                             
BYTE     DS    X                                                                
SCREEN   DS    H                                                                
TODAY    DS    XL3                 TODAY'S DATE - BINARY                        
         SPACE 3                                                                
REPTBL   DS    0CL2                                                             
*******  DC    C'MOTUWETHFRSASURAOT'                                            
         DC    C'MO'                                                            
         DC    C'TU'                                                            
         DC    C'WE'                                                            
         DC    C'TH'                                                            
         DC    C'FR'                                                            
         DC    C'SA'                                                            
         DC    C'SU'                                                            
         DC    C'OT'                                                            
         DC    C'RA'                                                            
         DC    X'FF'                                                            
         SPACE 2                                                                
SVCTBL   DS    0CL3                                                             
         DC    C'ARBNSI'                                                        
*****    DC    C'ARB'                                                           
*****    DC    C'NSI'                                                           
         DC    X'FF'                                                            
         EJECT                                                                  
FILE     DCB   DDNAME=FILE,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,LRECL=00901,BLKSIZE=09014                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         DS    0F                                                               
REC      DC    1000X'00'                                                        
         EJECT                                                                  
ROV      DSECT                                                                  
         DS    F                VB RECORD LENGTH                                
       ++INCLUDE REGENOVR                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031REUPOVR   08/31/00'                                      
         END                                                                    
