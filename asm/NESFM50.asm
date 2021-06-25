*          DATA SET NESFM50    AT LEVEL 025 AS OF 10/31/05                      
*PHASE T31C50A                                                                  
         TITLE 'T31C50  NV REPORT LETTER DEFINITION RECORDS'                    
T31C50   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C50                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         XC    SVCLT,SVCLT                                                      
*                                                                               
         LA    R2,NVLMEDH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         BAS   RE,VSUBMED          VALIDATE MEDIA/SUBMEDIA TYPE                 
*                                                                               
         XC    SMEDH,SMEDH                                                      
         MVC   SMEDH(9),=XL9'0900000000010000D5'                                
         LA    R2,SMEDH                                                         
*                                                                               
         GOTO1 VALIMED                                                          
         MVC   NVLMDN,MEDNM        MEDIA NAME                                   
         OI    NVLMDNH+6,X'80'                                                  
*                                                                               
         LA    R2,NVLCLTH                                                       
         CLI   SUBMED,0            ALL SUB MEDIA TYPES?                         
         BNE   VK10                                                             
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VK20                                                             
*                                                                               
VK10     DS    0H                                                               
         LA    R2,NVLCLTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VK20                                                             
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVCLT,BCLT                                                       
         MVC   NVLCLN,CLTNM        CLIENT NAME                                  
         OI    NVLCLNH+6,X'80'                                                  
*                                                                               
VK20     DS    0H                                                               
         LA    R6,KEY                                                           
         USING NVLRECD,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D43'                                                  
         MVC   NVLKAGY,AGENCY                                                   
         MVC   NVLKID,TWAORIG                                                   
         MVC   NVLKMED,SUBMED      MEDIA                                        
         MVC   NVLKCLT,SVCLT       CLIENT                                       
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE REC                                                                  
***********************************************************************         
VR       DS    0H                                                               
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'01'        REMOVE ALL ELEMENTS                          
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'03'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING NVLEL01,R6                                                       
*                                                                               
         XC    ELEM,ELEM           BUILD ADDRESS ELEMENT                        
         MVI   ELEM,X'01'                                                       
*                                                                               
         LA    R2,NVLADRH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    MISSFLD                                                          
*                                                                               
         BCTR  RE,0                                                             
         LA    RF,NVLADDR                                                       
         EX    RE,VRMVC                                                         
         LA    RE,3(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING NVLEL02,R6                                                       
*                                                                               
         XC    ELEM,ELEM           BUILD SIGNATURE ELEMENT                      
         MVI   ELEM,X'02'                                                       
*                                                                               
         LA    R2,NVLSIGH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    MISSFLD                                                          
*                                                                               
         BCTR  RE,0                                                             
         LA    RF,NVLSIGT                                                       
         EX    RE,VRMVC                                                         
         LA    RE,3(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING NVLEL03,R6                                                       
*                                                                               
         XC    ELEM,ELEM           BUILD DEPARTMENT ELEMENT                     
         MVI   ELEM,X'03'                                                       
*                                                                               
         LA    R2,NVLDEPH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    MISSFLD                                                          
*                                                                               
         BCTR  RE,0                                                             
         LA    RF,NVLDEPT                                                       
         EX    RE,VRMVC                                                         
         LA    RE,3(RE)                                                         
         STC   RE,ELEM+1                                                        
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VRX      DS    0H                                                               
         B     DR                                                               
         DROP  R6                                                               
*                                                                               
VRMVC    MVC   0(0,RF),8(R2)       ** EXECUTED                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY REC                                                                   
***********************************************************************         
DR       DS    0H                                                               
         XC    NVLADR,NVLADR                                                    
         XC    NVLSIG,NVLSIG                                                    
         XC    NVLDEP,NVLDEP                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        ADDRESS                                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,NVLADRH                                                       
         ZIC   R5,1(R6)                                                         
         SHI   R5,3                                                             
         EX    R5,DRMVC                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        SIGNATURE                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,NVLSIGH                                                       
         ZIC   R5,1(R6)                                                         
         SHI   R5,3                                                             
         EX    R5,DRMVC                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'        DEPARTMENT                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,NVLDEPH                                                       
         ZIC   R5,1(R6)                                                         
         SHI   R5,3                                                             
         EX    R5,DRMVC                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
DRMVC    MVC   8(0,R2),2(R6)       ** EXECUTED                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUB MEDIA TYPE                                                       
***********************************************************************         
VSUBMED  NTR1                                                                   
         LA    R2,NVLMEDH                                                       
         LA    R3,SMEDTAB                                                       
         MVI   SUBMED,C'N'                                                      
*                                                                               
         CLC   NVLMED(3),=C'ALL'                                                
         BNE   *+12                                                             
         MVI   SUBMED,0                                                         
         B     EXIT                                                             
*                                                                               
VSM10    DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    INVLFLD                                                          
*                                                                               
         CLC   0(1,R3),8(R2)       VALID SUBMEDIA                               
         BE    VSUBMEDX                                                         
         LA    R3,1(R3)                                                         
         B     VSM10                                                            
*                                                                               
VSUBMEDX DS    0H                                                               
         MVC   SUBMED,8(R2)                                                     
         B     EXIT                                                             
*                                                                               
SMEDTAB  DC    C'N'                NETWORK                                      
         DC    C'C'                CABLE                                        
         DC    C'S'                SYNDICATION                                  
         DC    C'D'                RADIO                                        
         DC    C'H'                HISPANIC                                     
         DC    C'O'                OTHER                                        
         DC    X'FF'                                                            
SMEDTABX EQU   *                                                                
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
MYERR    GOTO1 ERREX2                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM90D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENNVL                                                       
         EJECT                                                                  
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                                                                               
SUBMED   DS    CL1                 SUB MEDIA TYPE                               
SMEDH    DS    CL9                                                              
SVCLT    DS    XL2                 CLIENT                                       
*                                                                               
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025NESFM50   10/31/05'                                      
         END                                                                    
