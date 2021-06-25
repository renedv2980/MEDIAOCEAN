*          DATA SET TAGEN3D    AT LEVEL 019 AS OF 04/08/14                      
*PHASE T7023DA                                                                  
         TITLE 'T7023D - ATTENTION LIST'                                        
T7023D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7023D                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
ATT10    GOTO1 INITIAL,DMCB,PFTABLE                                             
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BE    VK                                                               
*                                                                               
ATT20    CLI   MODE,LISTRECS                                                    
         BNE   ATT30                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
ATT30    CLI   MODE,PRINTREP                                                    
         BNE   ATTX                                                             
         XC    KEY,KEY             ENSURE REPORT FROM BEGINING                  
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
ATTX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE THE KEY                                                              
*                                                                               
VK       MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,SATAGYH     VALIDATE AGENCY                  
         MVC   AIO,AIO1                                                         
         LA    R2,SATSTRH          START AT?                                    
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         ZIC   R3,5(R2)                                                         
         SH    R3,=H'1'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   TIQSTART(0),8(R2)                                                
*                                                                               
VK10     OI    4(R2),X'20'                                                      
         LA    R2,SATOPTSH         OPTIONS                                      
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         CLI   4(R1),0             INVALID INPUT                                
         BE    INVERR                                                           
         ZIC   R0,4(R1)                                                         
*                                                                               
VK20     DS    0H                                                               
*                                                                               
VK50     LA    R3,SCANNEXT                                                      
         BCT   R0,VK20                                                          
*                                                                               
VK60     OI    4(R2),X'20'                                                      
         XC    KEY,KEY             DEFAULT TO EQUAL ZERO                        
         BAS   RE,INIT                                                          
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP SYSIO                                                           
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
         MVI   TIREAD,TLATCDQ                                                   
         MVC   TIFAGY,TGAGY        FILTER ON AGENCY                             
*                                                                               
INITX    B     XIT                                                              
*                                                                               
         SPACE 4                                                                
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,16           GET BACK CONTROL AFTER                       
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15               1 FULL PAGE                              
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         EDIT  COUNTER,(8,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(22,R1),=C'ATTENTION CODE RECORDS'                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS SYSIO RECORDS                                                  
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BE    PRREC                                                            
*                                                                               
LRHX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RECORD                                                         
*                                                                               
PRREC    DS    0H                                                               
         USING LISTD,R2                                                         
         L     R4,TIAREC           ADDRESS OF RECORD                            
         USING TLATD,R4                                                         
         MVC   ATCODE,TLATATT      ATTN CODE                                    
*                                                                               
         MVC   ATNAME,SPACES       ATTENTION NAME                               
         MVC   AIO,TIAREC          SET IO ADDRESS FOR GETL                      
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=C'A')                                              
         BNE   PRR05                                                            
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         ZIC   R1,TAFNLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ATNAME(0),TAFNNAME                                               
*                                                                               
PRR05    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   PRR10                                                            
         GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         AP    COUNTER,=P'1'                                                    
         B     PRRX                                                             
*                                                                               
PRR10    CLI   LISTNUM,15          IF END OF PAGE                               
         BNE   PRR20               BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SATSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PRR20    GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRRX     B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
PFTABLE  DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'AGY     ',CL8'DISPLAY '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'ATTENTION LIST'                                          
         SSPEC H2,32,C'--------------'                                          
         SPACE 1                                                                
         SSPEC H4,1,C'ATTENTION'                                                
         SSPEC H4,15,C'NAME'                                                    
         SPACE 1                                                                
         SSPEC H5,1,C'---------'                                                
         SSPEC H5,15,C'----'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
ATCODE   DS    CL2                                                              
         DS    CL12                                                             
ATNAME   DS    CL36                                                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR3DD                                                       
         EJECT                                                                  
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
*                                                                               
         SPACE 2                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019TAGEN3D   04/08/14'                                      
         END                                                                    
