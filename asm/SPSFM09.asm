*          DATA SET SPSFM09    AT LEVEL 014 AS OF 04/23/03                      
*PHASE T21709A                                                                  
         TITLE 'T21709  NV REPORT LETTER DEFINITION RECORDS'                    
T21709   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21709                                                         
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
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D43'                                                  
         MVC   KEY+2(2),AGENCY                                                  
         MVC   KEY+4(2),TWAORIG                                                 
         XC    NVLMDN,NVLMDN                                                    
         OI    NVLMDNH+6,X'80'                                                  
         XC    NVLCLN,NVLCLN                                                    
         OI    NVLCLNH+6,X'80'                                                  
         LA    R2,NVLMEDH          VALIDATE MEDIA IF ANY                        
         CLI   5(R2),3                                                          
         BL    VK10                                                             
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK10                                                             
         CLI   NVLCLTH+5,0                                                      
         BE    EXIT                                                             
         CLI   NVLCLTH+5,3                                                      
         BNE   *+14                                                             
         CLC   =C'ALL',NVLCLT                                                   
         BE    EXIT                                                             
         LA    R2,NVLCLTH                                                       
VK09     MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VK10     MVC   SVKEY,KEY                                                        
         GOTO1 VALIMED                                                          
         CLI   QMED,C'C'                                                        
         BE    VK09                                                             
         MVC   NVLMDN,MEDNM                                                     
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVC   KEY+6(1),QMED                                                    
         LA    R2,NVLCLTH          VALIDATE CLIENT IF ANY                       
         CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    EXIT                                                             
         MVC   SVKEY,KEY                                                        
         GOTO1 VALICLT                                                          
         MVC   NVLCLN,CLTNM                                                     
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVC   KEY+7(2),BCLT                                                    
         B     EXIT                                                             
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,1            REMOVE ALL ELEMENTS                          
         GOTO1 REMELEM                                                          
         MVI   ELCODE,2                                                         
         GOTO1 REMELEM                                                          
         MVI   ELCODE,3                                                         
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM           BUILD ADDRESS ELEMENT                        
         LA    R6,ELEM                                                          
         USING NVLEL01,R6                                                       
         MVI   ELEM,1                                                           
         LA    R2,NVLADRH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    VRMISERR                                                         
         BCTR  RE,0                                                             
         LA    RF,NVLADDR                                                       
         EX    RE,VRMVC                                                         
         LA    RE,3(RE)                                                         
         STC   RE,ELEM+1                                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM           BUILD SIGNATURE ELEMENT                      
         LA    R6,ELEM                                                          
         USING NVLEL02,R6                                                       
         MVI   ELEM,2                                                           
         LA    R2,NVLSIGH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    VRMISERR                                                         
         BCTR  RE,0                                                             
         LA    RF,NVLSIGT                                                       
         EX    RE,VRMVC                                                         
         LA    RE,3(RE)                                                         
         STC   RE,ELEM+1                                                        
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM           BUILD DEPARTMENT ELEMENT                     
         LA    R6,ELEM                                                          
         USING NVLEL03,R6                                                       
         MVI   ELEM,3                                                           
         LA    R2,NVLDEPH                                                       
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BZ    VRMISERR                                                         
         BCTR  RE,0                                                             
         LA    RF,NVLDEPT                                                       
         EX    RE,VRMVC                                                         
         LA    RE,3(RE)                                                         
         STC   RE,ELEM+1                                                        
         GOTO1 ADDELEM                                                          
         B     EXIT                                                             
         SPACE                                                                  
VRMISERR MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         SPACE                                                                  
VRMVC    MVC   0(0,RF),8(R2)       ** EXECUTED                                  
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
*                                                                               
DR       L     R6,AIO              ADDRESS                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NVLEL01,R6                                                       
         ZIC   R5,1(R6)                                                         
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         XC    NVLADR,NVLADR                                                    
         FOUT  NVLADRH,NVLADDR,(R5)                                             
*                                                                               
         MVI   ELCODE,2            SIGNATURE                                    
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NVLEL02,R6                                                       
         IC    R5,1(R6)                                                         
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         XC    NVLSIG,NVLSIG                                                    
         FOUT  NVLSIGH,NVLSIGT,(R5)                                             
*                                                                               
         MVI   ELCODE,3            DEPARTMENT                                   
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NVLEL03,R6                                                       
         IC    R5,1(R6)                                                         
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         XC    NVLDEP,NVLDEP                                                    
         FOUT  NVLDEPH,NVLDEPT,(R5)                                             
         B     EXIT                                                             
*                                                                               
DRRECERR LA    R2,CONACTH                                                       
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         EJECT                                                                  
CLRSCRN  NTR1                                                                   
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         B     EXIT                                                             
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMF9D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENNVL                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPSFM09   04/23/03'                                      
         END                                                                    
