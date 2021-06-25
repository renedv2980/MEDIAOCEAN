*          DATA SET TAGENAC    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T702ACA                                                                  
         TITLE 'T702AC - FILTER-LIST  LIST'                                     
T702AC   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702AC                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   LST30                                                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     LST40                                                            
         SPACE 1                                                                
LST30    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P                                                             
         SPACE 1                                                                
LST40    BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SGLSTRH          START AT SPECIFIC LIST                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VK10                                                             
         NI    SGLOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART   START FROM BEGINING                          
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   TIQSTART(8),8(R2)                                                
         OI    4(R2),X'20'         VALIDATED                                    
         SPACE 1                                                                
VK10     LA    R2,SGLFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SGLFMTH+4,X'DF'                                                  
         SPACE 1                                                                
         MVI   RDSEQ,C'C'          IF NO INPUT                                  
         CLI   5(R2),0             DEFAULT TO ALPHA CODE SEQUENCE               
         BE    VK15                                                             
         CLI   5(R2),1             ONLY ONE CHARACTER INPUT ALLOWED             
         BH    INVERR                                                           
         CLI   8(R2),C'C'          INPUT MUST BE C (CODE)                       
         BE    *+12                                                             
         CLI   8(R2),C'N'          OR N (NAME)                                  
         BNE   INVERR                                                           
         MVC   RDSEQ,8(R2)                                                      
VK15     MVI   TIREAD,TLGLCDQ      SET TO READ GENERAL LIST RECORDS             
         CLI   RDSEQ,C'N'                                                       
         BNE   VK20                UNLESS READ OPTION IS SET FOR                
         MVI   TIREAD,TLGLNCDQ     NAME INSTEAD OF CODE                         
         SPACE 1                                                                
VK20     LA    R2,SGLOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         OI    4(R2),X'20'         VALIDATED                                    
         SPACE 1                                                                
         BAS   RE,INIT             RE-INITIALIZE LIST                           
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIFGLTY,TLGLTYPF    SET FILTER LIST TYPE                         
         MVI   TIREAD,TLGLCDQ      SET TO READ GENERAL LIST RECORDS             
         CLI   RDSEQ,C'N'                                                       
         BNE   XIT                 UNLESS READ OPTION IS SET FOR                
         MVI   TIREAD,TLGLNCDQ     NAME INSTEAD OF CODE                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         LA    R0,IOHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         SPACE 1                                                                
         MVI   NLISTS,16           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15           BACK AFTER 1 FULL PAGE                       
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         EDIT  COUNTER,(3,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(12,R1),=C'LIST RECORDS'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
         SPACE 1                                                                
         USING LINED,R2            R2=A(OUTPUT AREA)                            
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   IOHX                                                             
         L     R4,TIAREC           R4=A(RECORD)                                 
         USING TLGLD,R4                                                         
         MVC   LINLST,TLGLLST      LIST CODE                                    
         SPACE 1                                                                
         MVC   LINNAME,TINAME      LIST NAME                                    
         SPACE 1                                                                
         MVI   ELCODE,TAGLELQ      SET TO GET GENERAL LIST ELEMENTS             
         SPACE 1                                                                
         L     R3,AIO2             R3=A(UNSCAN BLOCK)                           
         XR    R0,R0               COUNT N'OUTPUT ENTRIES                       
         BAS   RE,GETEL                                                         
         B     *+8                                                              
IOH10    BAS   RE,NEXTEL                                                        
         BNE   IOH20                                                            
         USING TAGLD,R4                                                         
         MVC   0(30,R3),SPACES     PRE-CLEAR THIS ENTRY                         
         SPACE 1                                                                
         ZIC   R1,TAGLLEN          SET FOR EXECUTED MOVE                        
         SH    R1,=AL2(TAGLLNQ+1)                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAGLDATA    MOVE DATA TO BLOCK                           
         SPACE 1                                                                
         AH    R0,=H'1'            BUMP COUNT                                   
         LA    R3,30(R3)           BUMP TO NEXT ENTRY IN BLOCK                  
         CH    R0,=AL2(4000/30)    IF STILL HAVE ROOM                           
         BL    IOH10               LOOK FOR MORE ELEMENTS                       
         SPACE 1                                                                
IOH20    LTR   R0,R0               TEST SOMETHING FOUND                         
         BZ    IOH30                                                            
         MVC   BLOCK(80),SPACES                                                 
         GOTO1 UNSCAN,DMCB,((R0),AIO2),(C'C',BLOCK),0,(20,C' $LT')              
         MVC   LINDATA,BLOCK                                                    
         SPACE 1                                                                
IOH30    CLI   MODE,PRINTREP                                                    
         BNE   IOH40                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     IOHX                                                             
         SPACE 1                                                                
IOH40    CLI   LISTNUM,15          END OF 1 PAGE                                
         BNE   IOH50                                                            
         MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SGLSELH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
IOH50    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         SPACE 1                                                                
IOHX     B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         B     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,33,C'FILTER LIST LISTING'                                     
         SSPEC H2,33,C'-------------------'                                     
         SPACE 1                                                                
         SSPEC H4,2,C'CODE      NAME'                                           
         SSPEC H5,2,C'----      ----'                                           
         SSPEC H4,50,C'DATA'                                                    
         SSPEC H5,50,C'----'                                                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LINED    DSECT                                                                  
         DS    CL1                                                              
LINLST   DS    CL8                                                              
         DS    CL2                                                              
LINNAME  DS    CL36                                                             
         DS    CL2                                                              
LINDATA  DS    CL26                                                             
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRACD                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAGENAC   05/01/02'                                      
         END                                                                    
