*          DATA SET TAGENE5    AT LEVEL 007 AS OF 12/30/13                      
*PHASE T702E5A,*                                                                
         TITLE 'T702E5 - TRUST DISPLAY'                                         
T702E5   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         NMOD1 0,T702E5                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=SYSTEM STORAGE AREA                       
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INTIALIZE                                    
         TM    TGSYSTAT,TASYSPID                                                
         BZ    INIT10                                                           
         MVC   TRUPHED(13),=C'Performer Pid'                                    
         OI    TRUPHEDH+6,X'80'                                                 
         MVC   TRUTHED(11),=C'Trustee Pid'                                      
         OI    TRUTHEDH+6,X'80'                                                 
*                                                                               
         SPACE 1                                                                
INIT10   CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BAS   RE,VKEY             VALIDATE KEY                                 
         SPACE 1                                                                
         CLI   MODE,DISPREC                                                     
         BNE   *+8                                                              
         BAS   RE,DREC             DISPLAY RECORD                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE KEY FOR PERFORMER'S W4 RECORD                           
         SPACE 1                                                                
VKEY     NTR1                                                                   
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    VK10                                                             
         CLI   TRUPSSH+5,6                                                      
         BH    VK10                                                             
         MVC   TGPID,TRUPSS                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK10                                                             
         MVC   TRUPSS,TGSSN                                                     
         MVI   TRUPSSH+5,9                                                      
VK10     GOTO1 RECVAL,DMCB,TLW4CDQ,(X'28',TRUPSSH),TRUPSSNH                     
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    XIT                                                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   TRUPSS,SPACES                                                    
         MVC   TRUPSS(L'TGPID),TGPID                                            
         MVI   TRUPSSH+5,6                                                      
         OI    TRUPSSH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 1                                                                
DREC     NTR1                                                                   
         TWAXC TRUTSSH,TRUTSSNH,PROT=Y                                          
         TWAXC TRUEMP1H,TRUAWH3H,PROT=Y                                         
         SPACE 1                                                                
         L     R4,AIO                  R4=A(W4 RECORD)                          
         MVI   ELCODE,TAWXELQ                                                   
         BAS   RE,GETEL                W4=A(EXTRA DETAILS ELEMENT FOR           
         BNE   XIT                          FOR W4)                             
         SPACE 1                                                                
         USING TAWXD,R4                                                         
         OC    TAWXTSSN,TAWXTSSN                                                
         BE    DREC05                                                           
         MVC   TRUTSS,TAWXTSSN         DISPLAY TRUSTEE'S SSN AND NAME           
         MVI   TRUTSSH+5,L'TAWXTSSN                                             
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',TRUTSSH),TRUTSSNH                     
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#?                           
         BZ    DREC02                                                           
         GOTO1 SSNPACK,DMCB,TAWXTSSN,TGPID                                      
         MVC   TRUTSS,SPACES                                                    
         MVC   TRUTSS(L'TGPID),TGPID                                            
         MVI   TRUTSSH+5,6                                                      
         OI    TRUTSSH+6,X'80'                                                  
*                                                                               
DREC02   MVC   AIO,AIO1                                                         
         MVC   TGSSN,TRUPSS                                                     
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#?                           
         BZ    DREC05                                                           
         CLI   TRUPSSH+5,6                                                      
         BH    DREC05                                                           
         MVC   TGSSN,SPACES                                                     
         MVC   TGPID,TRUPSS                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         SPACE 1                                                                
DREC05   LA    R2,TRUEMP1H             R2=A(1ST EMPLOYER SCREEN FIELD)          
         LA    R3,TAWXHTP              R3=A(1ST COL/WITH FLD IN ELEM)           
         LA    R5,CODES                R5=A(1ST EMPLOYER CODE)                  
         SPACE 1                                                                
DREC10   CLI   0(R5),X'FF'             IF AT END OF EMPLOYER CODES ...          
         BE    XIT                     DONE                                     
         SPACE 1                                                                
         OC    0(L'TAWXHTP,R3),0(R3)   DOES COLLECTED & WITHHELD EXIST          
         BZ    DREC20                  FOR THIS EMPLOYER CODE?                  
         MVC   8(L'CODES,R2),0(R5)     YES - MOVE CODE AND AMOUNT TO            
         BAS   RE,BUMP                 SCREEN                                   
         EDIT  (4,(R3)),(10,8(R2)),2,MINUS=YES                                  
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
DREC20   AHI   R5,L'CODES              BUMP TO NEXT EMPLOYER CODE AND           
         AHI   R3,L'TAWXHTP            COLLECTED/WITHELD AMOUNT                 
         B     DREC10                                                           
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE BUMPS TO NEXT SCREEN FIELD                               
         SPACE 1                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
*              TABLE OF EMPLOYER CODES                                          
*              (CORRESPONDS TO ORDER IN TAWXD ELEMENT)                          
         SPACE 1                                                                
CODES    DS    0CL3                                                             
         DC    CL3'TP'                                                          
         DC    CL3'P+'                                                          
         DC    CL3'PP'                                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
*              PF KEY TABLE                                                     
         SPACE 1                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'W4      ',CL8'DISPLAY '                               
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRE5D          SCREEN                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007TAGENE5   12/30/13'                                      
         END                                                                    
