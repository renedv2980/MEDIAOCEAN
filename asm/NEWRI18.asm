*          DATA SET NEWRI18    AT LEVEL 052 AS OF 10/29/97                      
*********************************************************                       
*                                                       *                       
*   LEVEL NUMBER DOESN'T MATCH BUT THIS IS LAST VERSION *                       
*                                                       *                       
*********************************************************                       
*PHASE T32018A,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32018 - COMMERCIAL CHECKING'                                   
T32018   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NE18**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T320FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          ANETWS1=WORKING STORAGE                      
         USING MYD,R7                                                           
         MVC   NBACLI,ANETWS3      CLIENT RECORD IN W23                         
         ST    R2,RELO                                                          
         MVI   NDTITLE,C' '                                                     
         MVC   NDTITLE+1(L'NDTITLE-1),NDTITLE                                   
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BE    RP100                                                            
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    ED100                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              EDIT ROUTINES                                                    
         SPACE 3                                                                
ED100    MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         MVI   NBDATA,C'U'         UNIT RECORDS ONLY                            
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS ARE OPTIONAL                
*                                                                               
         LA    R2,SPLPROH          PRODUCT                                      
         MVI   MGLOPTS,C'Y'                                                     
         NETGO NVPRD,DMCB,SPLPRON                                               
         CLI   5(R2),0                                                          
         BE    *+18                                                             
         CLC   NBSELPRD,=C'POL'                                                 
         BE    *+8                                                              
         MVI   MGLOPTS,C'N'        DON'T PRINT PRODUCT IN REPORT                
         OI    SPLPRONH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLESTH          ESTIMATE                                     
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
         CLC   8(2,R2),=C'NO'                                                   
         BNE   *+8                                                              
         MVI   MGLOPTS+1,C'Y'      DON'T PRINT ESTIMATE IN REPORT               
*                                                                               
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB                                                    
         CLC   =C'ALL,',8(R2)                                                   
         BNE   *+12                                                             
         LA    RE,MEDAREA                                                       
         ST    RE,NBANBUFF                                                      
*                                                                               
         LA    R2,SPLSTRTH         START DATE                                   
         CLC   =C'ALL',SPLEST                                                   
         BE    *+12                                                             
         CLI   NBSELESE,0                                                       
         BE    *+8                                                              
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLENDDH         END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                  REPORT INITIALIZATION                        
         SPACE                                                                  
RP100    GOTO1 CALLOV,DMCB,0,X'D9031EB7'  LOAD T31EB7 (GLOBAL STORAGE)          
         L     R6,DMCB                                                          
         USING GLOBALD,R6                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'  LOAD T00A3A (DRIVER)                  
         MVC   DRIVER,DMCB                                                      
         GOTO1 CALLOV,DMCB,0,X'D9000A41'  LOAD T00A41 (NETWORK DRIVER)          
         MVC   GLASYSDR,DMCB                                                    
         GOTO1 CALLOV,DMCB,0,X'D9032008'  LOAD T32008 (DPG PHASE)               
         MVC   GLAPROG,DMCB                                                     
         MVC   GLOPTS+1(2),MGLOPTS                                              
         SPACE 1                                                                
         ST    RC,GLAWORKD                                                      
         MVI   GLTWORKD,GLTSPOOL                                                
         LA    R2,HEDSPECS                                                      
         ST    R2,SPECS                                                         
         LA    R2,HOOK                                                          
         ST    R2,GLAHOOK                                                       
         MVI   GLFHEADL,10                                                      
         SPACE 3                                                                
*              NOW CONTROL NETIO                                                
         SPACE 1                                                                
         OI    NBSPLOPT,X'C0'      OPTION TO SPLIT EVEN IF POOL                 
         OI    GLINDS,X'02'      PRINT ALL DETAILS                              
         MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         GOTO1 DRIVER,DMCB,(R6)                                                 
         EJECT                                                                  
*              INPUT - PROGRAM I/O                                              
         SPACE 3                                                                
GU100    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BE    GU200                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    GU500                                                            
         CLI   NBERROR,0                                                        
         BE    GU100                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
GU200    DS    0H                                                               
         TM    NBUNITST,X'02'      IF MISSED                                    
         BO    GU100               SKIP                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'INPUT',NBAIO,C'DUMP',40,=C'1D'                
         MVI   GLMODE,GLINPUT                                                   
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     GU100                                                            
         SPACE 1                                                                
*                                  GO AND DO THE OUTPUT                         
GU500    MVI   GLMODE,GLOUTPUT                                                  
         GOTO1 DRIVER,DMCB,(R6)                                                 
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE SPECS                                                   
         SPACE 3                                                                
         PRINT NOGEN                                                            
HEDSPECS SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,48,C'COMMERCIAL CHECKING'                                     
         SSPEC H2,48,C'-------------------'                                     
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,53,PERIOD                                                     
         SSPEC H4,99,NETREP                                                     
         SSPEC H5,125,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*              HEADLINE ROUTINES, ETC                                           
*                                                                               
HOOK     NTR1                              HEAD HOOK                            
*                                                                               
         CLI   GLHOOK,GLHEAD                                                    
         BNE   HKXIT                                                            
         NETGO NVHEAD,DMCB                                                      
         SPACE 1                                                                
HKXIT    B     XIT                                                              
*                                                                               
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
*              LTORG                                                            
         LTORG                                                                  
         EJECT                                                                  
*              WORKING STORAGE FOR PROGRAM                                      
         SPACE 3                                                                
MYD      DSECT                                                                  
*                                                                               
RELO     DS    A                                                                
DRIVER   DS    A                                                                
STRTDATE DS    H                   START DATE                                   
ENDDATE  DS    H                   START DATE                                   
MGLOPTS  DS    CL1                 SINGLE PRODUCT REQUEST                       
         DS    CL1                 ALL ESTIMATES                                
MEDAREA  DS    1000C                                                            
*                                  AND MOVE TO GLOPTS AT RUN TIME               
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRIF8D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052NEWRI18   10/29/97'                                      
         END                                                                    
