*          DATA SET TAGEN31    AT LEVEL 013 AS OF 04/14/05                      
*PHASE T70231A                                                                  
         TITLE 'T70231 - STAFF LIST'                                            
T70231   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70231                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=TWAHOLE                                   
         USING WORKD,R7                                                         
         EJECT                                                                  
* MODE CONTROLLED ROUTINES                                                      
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
*                                                                               
         CLI   MODE,LISTRECS       IF MODE LISTRECS                             
         BNE   STA10                                                            
         LA    R2,LISTAR           POINT R2 TO LISTAR                           
         B     LR                  GO TO LR                                     
*                                                                               
STA10    CLI   MODE,PRINTREP       ELSE IF MODE PRINTREP                        
         BNE   STAX                                                             
         ZAP   PRCOUNT,=P'0'       CLEAR RECORD COUNT                           
         LA    RF,MYSPECS          SET REPORT SPECS                             
         ST    RF,SPECS                                                         
         LA    R2,P                POINT R2 TO PRINT LINE                       
         B     LR                  AND GO TO LR                                 
*                                                                               
STAX     B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VK       DS    0H                                                               
*                                                                               
         TM    SCRSTAT,RECCHG      IF RECORD CHANGED                            
         BZ    *+8                                                              
         NI    SSLUSERH+4,X'DF'    REVALIDATE ENTIRE KEY                        
*                                                                               
         LA    R2,SSLUSERH         VALIDATE USER ID                             
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SSLTYPEH+4,X'DF'                                                 
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VK3                                                              
         XC    MYUSER,MYUSER       SET USER ID TO ZEROS                         
         MVC   8(3,R2),=C'ALL'     MOVE 'ALL' INTO FIELD                        
         OI    6(R2),X'80'                                                      
         B     VK7                                                              
*                                                                               
VK3      CLC   =C'ALL',8(R2)       ELSE IF USER ID IS 'ALL'                     
         BNE   VK5                                                              
         XC    MYUSER,MYUSER       SET USER ID TO ZEROS                         
         B     VK7                                                              
*                                                                               
VK5      GOTO1 USERVAL,DMCB,(R2)   ELSE CALL USERVAL                            
         MVC   MYUSER,TGUSER       SAVE USER ID                                 
*                                                                               
VK7      OI    4(R2),X'20'                                                      
*                                                                               
VK10     LA    R2,SSLTYPEH         VALIDATE TYPE FILTER                         
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         NI    SSLSTRH+4,X'DF'                                                  
*                                                                               
         MVI   TYPEFILT,0          IF NOT ENTERED THEN ZERO                     
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 STAFVAL,DMCB,8(R2),0      VALIDATE IT                            
         BNE   ERRINV                                                           
         GOTO1 (RF),(R1),8(R2),TGCTSTLV  TEST IF USER CAN ACCESS IT             
         BNE   ERRINV                                                           
         MVC   TYPEFILT,8(R2)      SAVE TYPE FILTER                             
VK30     OI    4(R2),X'20'                                                      
*                                                                               
VK50     LA    R2,SSLSTRH          VALIDATE START AT KEY                        
         TM    4(R2),X'20'                                                      
         BO    VK60                                                             
         NI    SSLFMTH+4,X'DF'                                                  
*                                                                               
         XC    STARTAT,STARTAT     SET TO ZEROS IF NOTHING ENTERED              
         CLI   5(R2),0                                                          
         BE    VK55                                                             
*                                                                               
         ZIC   R3,5(R2)            ELSE EXTRACT KEY FROM FIELD                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   STARTAT(0),8(R2)                                                 
         OC    STARTAT,SPACES      PAD WITH SPACES                              
*                                                                               
VK55     OI    4(R2),X'20'                                                      
*                                                                               
VK60     LA    R2,SSLFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'                                                      
         BO    VK70                                                             
         NI    SSLOPTSH+4,X'DF'                                                 
*                                                                               
         MVI   RDSEQ,C'A'          DEFAULT TO ALPHA SEQUENCE                    
         OC    MYUSER,MYUSER                                                    
         BZ    *+8                                                              
         MVI   RDSEQ,C'C'          UNLESS ID INPUT, THEN DEF TO CODE            
         CLI   5(R2),0                                                          
         BE    VK65                                                             
         CLI   5(R2),1             ONLY 1 CHAR CAN BE INPUT                     
         BNE   ERRINV                                                           
         CLI   8(R2),C'A'                                                       
         BE    VK65                                                             
         MVI   RDSEQ,C'C'                                                       
         CLI   8(R2),C'C'          CODE SEQUENCE                                
         BE    VK65                                                             
         CLI   8(R2),C'X'          BAD RECORDS ONLY                             
         BNE   ERRINV                                                           
         CLI   TGCTSTTY,TASTTYPP   PROGRAMMERS ONLY                             
         BNE   ERRINV                                                           
*                                                                               
VK65     OI    4(R2),X'20'                                                      
*                                                                               
VK70     LA    R2,SSLOPTSH         VALIDATE OPTIONS                             
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
*                                                                               
         CLI   5(R2),0             NOTHING VALID AT THIS POINT                  
         BNE   ERRINV                                                           
*                                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
         BAS   RE,SETSYS           SET UP SYSIO BLOCK                           
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
* SET UP SYSIO DSECT WITH NECESSARY INFORMATION                                 
*                                                                               
SETSYS   NTR1                                                                   
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM                  
         MVC   TIQSTAFF,TGCTSTAF       GLOBAL STORAGE                           
*                                                                               
         CLI   RDSEQ,C'A'          IF ALPHA SEQUENCE REQUESTED                  
         BNE   SS10                                                             
         MVI   TIREAD,TLSTNCDQ     LIST BY STAFF NAME                           
         B     SS20                                                             
*                                                                               
SS10     MVI   TIREAD,TLSTCDQ      ELSE LIST BY STAFF CODE                      
*                                                                               
SS20     XC    TIFID,TIFID         FILTER ON USER ID                            
         MVC   TIFID(2),MYUSER                                                  
*                                                                               
         MVC   TIQSTART,STARTAT    SET START KEY                                
*                                                                               
SSX      B     XIT                                                              
         EJECT                                                                  
* LIST THE RECORDS                                                              
*                                                                               
LR       LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET A(COMFACS)                               
         MVC   TIKHOOK,SETLSTK     SET A(CONTINUE ROUTINE)                      
         XC    TIQSKEY,KEY         SET CONTINUE KEY                             
*                                                                               
         MVI   NLISTS,16           CALL SYSIO                                   
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15                                                        
*                                                                               
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   LRX                 THEN PRINT NUMBER OF AGENCIES                
         MVC   P(25),=C'NUMBER OF STAFF RECORDS :'                              
         EDIT  PRCOUNT,(7,P+26),ALIGN=LEFT,ZERO=NOBLANK                         
         GOTO1 CATCHIOS            LIMIT IOS                                    
         GOTO1 SPOOL,DMCB,(R8)     CALL SPOOL                                   
*                                                                               
         XC    CONSERV,CONSERV     AUTO CALL $DQU                               
         MVC   CONSERV(4),=C'$DQU'                                              
         OI    CONSERVH+6,X'80'                                                 
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
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         USING LISTD,R2                                                         
*                                                                               
         L     R4,TIAREC           R4 = A(RECORD)                               
         USING TLSTD,R4                                                         
*                                                                               
         OC    MYUSER,MYUSER       IF USER ID FILTER SPECIFIED                  
         BZ    *+14                                                             
         CLC   TLSTUSER,MYUSER     THEN SKIP DIFFERENT USER IDS                 
         BNE   PRX                                                              
*                                                                               
         XC    WORK,WORK           MOVE IN EBCDIC USER ID                       
         MVC   WORK+8(2),TLSTUSER                                               
         GOTO1 USERVAL,DMCB,(X'A0',WORK)                                        
         BE    *+16                                                             
         CLI   SSLFMT,C'X'                                                      
         BNE   PRX                                                              
         B     *+12                                                             
         CLI   SSLFMT,C'X'                                                      
         BE    PRX                                                              
         MVC   SLUSER,TGUSERID                                                  
         MVC   SLUSERN,TGNAME      USER ID NAME                                 
         MVC   SLSTAFF,TISTAFF     MOVE IN STAFF CODE                           
*                                                                               
         MVI   ELCODE,TASTELQ      POINT R4 TO STAFF ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TASTD,R4                                                         
*                                                                               
         CLC   TWAORIG,TGUSER      CAN ALWAYS ACCESS YOURSELF                   
         BNE   *+14                                                             
         CLC   TGCTSTAF,TISTAFF                                                 
         BE    PR50                                                             
         GOTO1 STAFVAL,DMCB,TASTTYPE,TGCTSTLV  TEST IF USER CAN ACCESS          
         BNE   PRX                             THIS LEVEL                       
*                                                                               
PR50     CLI   TYPEFILT,0          IF TYPE FILTER SPECIFIED                     
         BE    *+14                                                             
         CLC   TASTTYPE,TYPEFILT   THEN SKIP DIFFERENT CATEGORIES               
         BNE   PRX                                                              
*                                                                               
         MVC   SLLAST,TASTLST      MOVE IN LAST NAME                            
         MVC   SLFIRST,TASTFST             FIRST NAME                           
         MVC   SLCAT,TASTTYPE              CATEGORY                             
         GOTO1 STAFVAL,DMCB,TASTTYPE,0                                          
         MVC   SLCATN,TGSTNAME             CATEGORY NAME                        
         DROP  R4                                                               
*                                                                               
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
*                                                                               
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   PR70                                                             
         GOTO1 CATCHIOS            LIMIT IOS                                    
         GOTO1 SPOOL,DMCB,(R8)     THEN CALL SPOOL                              
         AP    PRCOUNT,=P'1'       INCREMENT RECORD COUNT                       
         B     PRX                                                              
*                                                                               
PR70     CLI   LISTNUM,15          ELSE IF END OF PAGE                          
         BNE   PR80                BUT THERE ARE MORE RECS                      
         MVC   MYMSGNO1,OKNO           PUT MSG - HIT ENTER FOR NEXT             
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SSLSELH                                                       
         B     ERRXIT                                                           
*                                                                               
PR80     GOTO1 LISTMON             CALL LISTMON                                 
*                                                                               
PRX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,33,C'STAFF LIST'                                              
         SSPEC H2,33,C'----------'                                              
         SPACE 1                                                                
         SSPEC H4,1,C'USERID USERID NAME'                                       
         SSPEC H5,1,C'------ -----------'                                       
         SPACE 1                                                                
         SSPEC H4,23,C'STAFF    LAST NAME    FIRST NAME'                        
         SSPEC H5,23,C'-----    ---------    ----------'                        
         SPACE 1                                                                
         SSPEC H4,58,C'CATEGORY'                                                
         SSPEC H5,58,C'--------'                                                
         DC    X'00'                                                            
         EJECT                                                                  
* APPLICATION STORAGE PUT AT END OF TWA                                         
*                                                                               
WORKD    DSECT                                                                  
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
MYUSER   DS    XL2                 USER ID FOR FILTERING                        
TYPEFILT DS    C                   STAFF TYPE FOR FILTERING                     
STARTAT  DS    CL16                START AT KEY                                 
PRCOUNT  DS    PL4                 RECORD COUNT FOR REPORTS                     
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
LISTD    DSECT                                                                  
SLUSER   DS    CL6                 USER ID                                      
         DS    XL1                                                              
SLUSERN  DS    CL14                USER ID NAME                                 
         DS    XL1                                                              
SLSTAFF  DS    CL8                 STAFF CODE                                   
         DS    XL1                                                              
SLLAST   DS    CL12                LAST NAME                                    
         DS    XL1                                                              
SLFIRST  DS    CL12                FIRST NAME                                   
         DS    XL1                                                              
SLCAT    DS    CL1                 CATEGORY                                     
         DS    XL1                                                              
SLCATN   DS    CL16                CATEGORY NAME                                
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR31D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TAGEN31   04/14/05'                                      
         END                                                                    
