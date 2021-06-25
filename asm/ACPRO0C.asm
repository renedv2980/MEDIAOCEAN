*          DATA SET ACPRO0C    AT LEVEL 019 AS OF 12/03/09                      
*PHASE T60B0CA                                                                  
         TITLE 'T60B0C - OFFICE MAINT'                                          
T60B0C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B0C**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         GOTO1 VMODPTRS,DMCB,(X'80',POINTERS)                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         CLI   ACTNUM,ACTREP                                                    
         BE    *+8                                                              
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BZ    MODE3                                                            
         CLI   ACTNUM,ACTADD       TEST ADDING OFFICE                           
         BNE   *+8                                                              
         BAS   RE,VOFFC            VALIDATE AGAINST GENERAL ACCTG OFFC          
         SPACE 1                                                                
MODE3    BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,DREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE8    CLI   MODE,PRINTREP                                                    
         BNE   MODE10                                                           
         BAS   RE,PREP                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE10   GOTO1 CANWEDEL                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     NTR1                      OFFICE                                       
         LA    R2,PROOFFH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         LA    R1,1                SET OLF OFFICE LENGTH                        
         TM    COMPSTA4,X'01'      ON NEW OFFICES?                              
         BZ    *+8                 NO                                           
         LA    R1,2                YES, CHANGE LENGTH                           
*                                                                               
         EX    R1,*+8              CHECK APPRORIATE LENGTH                      
         B     *+8                                                              
         CLI   5(R2),0                                                          
         BNE   ERREND                                                           
*                                                                               
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF                                                 
         MVC   ACOGCUL,CUL                                                      
         MVC   ACOGOFC,WORK                                                     
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE OFFICE CODE AGAINST GENERAL ACCOUNTING OFFICE                        
*                                                                               
VOFFC    NTR1  ,                                                                
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY        TUCK THE OFFICE KEY AWAY                     
         LA    R4,KEY                                                           
         USING OFFRECD,R4                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUL                                                      
         LA    R2,PROOFFH                                                       
         GOTO1 ANY                                                              
         MVC   OFFKOFF,WORK                                                     
         MVI   ERROR,GENOFERR                                                   
         GOTO1 HIGH                                                             
         CLC   OFFKEY,KEYSAVE      TEST IF CODE FOUND                           
         BNE   ERREND              REJECT CODE                                  
         L     R4,AIO                                                           
         TM    OFFKEY+(ACSTATUS-ACKEYD),OFFSLIST TEST FOR LIST RECORD           
         BO    ERREND              YES-NOT A VALID CODE EITHER                  
         SPACE 1                                                                
VOFFC2   MVC   KEY(L'ACOGKEY),SAVEKEY1 RESTORE OFFICE KEY                       
         MVC   AIO,AIO1            RESTORE IO AREA POINTER                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VADIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,PROOFFNH                                                      
         GOTO1 ANY                                                              
         GOTO1 NAMEIN                                                           
         GOTO1 PERSIN                                                           
         SPACE 1                                                                
         LA    R2,PROOGRH          OFFICE GROUP IS OPTIONAL                     
         MVI   ELCODE,ACGPELQ                                                   
         GOTO1 REMELEM                                                          
         CLI   5(R2),0                                                          
         BE    VREC2                                                            
         MVC   AIO,AIO2                                                         
         MVC   SAVEKEY1,KEY                                                     
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALOG                                                            
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         MVI   OPTION,0                                                         
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING ACGPD,R6                                                         
         MVI   ACGPEL,ACGPELQ                                                   
         MVI   ACGPLEN,ACGPLENQ                                                 
         MVC   ACGPCODE,8(R2)                                                   
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VREC2    LA    R2,PRODCLH                                                       
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         LLC   R0,5(R2)                                                         
         GOTO1 TSTAN,8(R2)                                                      
*                                                                               
VREC4    GOTO1 BLDFFT,DMCB,(R2),FFTTDCLN                                        
*                                                                               
         LA    R2,PROLCLH                                                       
         CLI   5(R2),0                                                          
         BE    VREC6                                                            
         LLC   R0,5(R2)                                                         
         GOTO1 TSTAN,8(R2)                                                      
*                                                                               
VREC6    GOTO1 BLDFFT,DMCB,(R2),FFTTLCLN                                        
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         USING ACOGKEY,R4                                                       
         MVC   PROOFF,ACOGOFC                                                   
         LA    R2,PROOFFH                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     NTR1                                                                   
         LA    R2,PROOFFNH                                                      
         GOTO1 NAMEOUT                                                          
         GOTO1 PERSOUT                                                          
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
*                                                                               
         MVC   PROOGR,SPACES       OPTIONAL OFFICE GROUP                        
         MVC   PROOGRN,SPACES                                                   
         MVC   PRODCL,SPACES                                                    
         MVC   PROLCL,SPACES                                                    
         OI    PROOGRH+6,X'80'                                                  
         OI    PROOGRNH+6,X'80'                                                 
         OI    PRODCLH+6,X'80'                                                  
         OI    PROLCLH+6,X'80'                                                  
*                                                                               
         LA    R2,PROOGRH                                                       
         MVI   ELCODE,ACGPELQ                                                   
         BAS   RE,GETELIO                                                       
         BNE   DREC2                                                            
         USING ACGPD,R6                                                         
         SPACE 1                                                                
         MVC   8(1,R2),ACGPCODE    IF ACTIVE, SHOW IT                           
         MVC   SAVEKEY1,KEY                                                     
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         MVI   ACOGSREC,ACOGOG                                                  
         MVC   ACOGCODE(2),ACGPCODE   MOVE CODE AND SPARE (ZERO)                
         GOTO1 READ                                                             
         GOTO1 SETNAME,DMCB,AIO,PROOGRN                                         
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
*                                                                               
DREC2    LA    R2,PRODCL                                                        
         GOTO1 GETFFT,DMCB,(R2),FFTTDCLN                                        
*                                                                               
         LA    R2,PROLCL                                                        
         GOTO1 GETFFT,DMCB,(R2),FFTTLCLN                                        
         B     XIT                                                              
         EJECT                                                                  
*              PRINT OFFICE REPORT                                              
         SPACE 3                                                                
PREP     NTR1                                                                   
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
*                                                                               
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY     READ OFFICE RECORDS                          
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF    OFFICE SUB-RECORD                            
         MVC   ACOGCUL,CUL                                                      
         GOTO1 HIGH                                                             
         B     PREP4                                                            
         SPACE 1                                                                
PREP2    GOTO1 SEQ                                                              
         SPACE 1                                                                
PREP4    CLC   ACOGKEY(ACOGOFC-ACOGKEY),KEYSAVE  CHECK C/B                      
         BNE   PREPX                                                            
         SPACE 1                                                                
         LA    R2,P                                                             
         USING PRTD,R2                                                          
         MVC   PRTOFF,ACOGOFC      EXTRACT OFFICE CODE                          
         GOTO1 SETNAME,DMCB,AIO,WORK                                            
         MVC   PRTNAME,WORK        EXTRACT OFFICE NAME                          
         MVI   ELCODE,ACGPELQ      GET PRODUCTION GROUP ELEMENT                 
         BAS   RE,GETELIO                                                       
         BNE   *+10                NOT IN A GROUP                               
         USING ACGPD,R6                                                         
         MVC   PRTOG,ACGPCODE                                                   
         GOTO1 GETFFT,DMCB,PRTDCLM,FFTTDCLN                                     
         GOTO1 GETFFT,DMCB,PRTLCLM,FFTTLCLN                                     
         MVI   ALLOWLIN,2                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PREP2               GET NEXT OFFICE                              
*                                                                               
PREPX    B     XIT                                                              
         EJECT                                                                  
*              HEAD HOOK                                                        
         SPACE 3                                                                
HOOK     NTR1                                                                   
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         CLI   BOXOPT,C'N'                                                      
         BE    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         MVI   PRTLBOX-PRTD(R2),C'L'                                            
         MVI   PRTBOX1-PRTD(R2),C'C'                                            
         MVI   PRTBOX2-PRTD(R2),C'C'                                            
         MVI   PRTBOX3-PRTD(R2),C'C'                                            
         MVI   PRTBOX4-PRTD(R2),C'C'                                            
         MVI   PRTRBOX-PRTD(R2),C'R'                                            
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
GETFFT   NTR1                                                                   
         L     R2,0(R1)                                                         
         LA    R6,ELEMENT          READ THE ELEMENT                             
         XC    ELEMENT,ELEMENT                                                  
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVC   FFTTYPE,7(R1)                                                    
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('FFTELQ',AIO),(1,FFTTYPE)              
         CLI   12(R1),0                                                         
         BNE   GETFFTX                                                          
*                                                                               
         L     R6,12(R1)           OUTPUT TEXT                                  
         XR    RF,RF                                                            
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),FFTDATA                                                  
*                                                                               
GETFFTX  XIT1                                                                   
         EJECT                                                                  
BLDFFT   NTR1                                                                   
         L     R2,0(R1)                                                         
         LA    R6,ELEMENT          DELETE CURRENT ELEMENT                       
         XC    ELEMENT,ELEMENT                                                  
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVC   FFTTYPE,7(R1)                                                    
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),('FFTELQ',AIO),(1,FFTTYPE)              
         CLI   5(R2),0                                                          
         BE    BLDFFTX                                                          
*                                                                               
         MVC   FFTDLEN,5(R2)                                                    
         MVC   FFTDATA(L'PRODCL),SPACES                                         
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FFTDATA(0),8(R2)                                                 
         LA    RE,FFTLN1Q+2(RE)                                                 
         STC   RE,FFTLN                                                         
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,FFTELD                              
*                                                                               
BLDFFTX  XIT1                                                                   
         DROP  R6                                                               
*              SUPPORTING SUBROUTINES                                           
TSTAN    ST    RE,SAVERE                                                        
         MVI   ERROR,CLAIMSIX                                                   
         CHI   R0,6                MUST BE 6 BYTES                              
         BNE   ERREND                                                           
*                                                                               
         MVI   ERROR,CLAIMOFF                                                   
         XR    RF,RF                                                            
         TM    COMPSTA4,CPYSOFF2   2 CHAR OFFICE?                               
         BZ    *+8                                                              
         AHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),PROOFF      FIRST TWO POSITIONS BUT BE OFFICE            
         BNE   ERREND                                                           
         AHI   RF,1                                                             
         SR    R0,RF                                                            
         LA    R1,0(R1,RF)                                                      
         MVI   ERROR,ALPHATWO                                                   
*                                                                               
TSTAN2   CLI   0(R1),C'0'                                                       
         BL    ERREND                                                           
         CLI   0(R1),C'9'                                                       
         BH    ERREND                                                           
         LA    R1,1(R1)            BUMP TO NEXT                                 
         BCT   R0,TSTAN2                                                        
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
SAVERE   DS    A                                                                
*                                                                               
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
*              SPECS FOR HEADINGS ETC                                           
         SPACE 1                                                                
MYSPECS  DS    0F                                                               
         SSPEC H1,2,CREATED        SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,47,C'OFFICE LISTING'                                          
         SSPEC H2,47,C'--------------'                                          
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
*                                                                               
         SSPEC H8,22,C'OF'                                                      
         SSPEC H8,26,C'OFFICE NAME'                                             
         SSPEC H8,62,C'OFG'                                                     
         SSPEC H8,66,C'DCLAIM'                                                  
         SSPEC H8,73,C'LCLAIM'                                                  
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECTS ARE HIDDEN IN HERE                                        
         SPACE 3                                                                
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROFCD                                                       
         SPACE 1                                                                
POINTERS DS    CL(8*54+1)          PASSIVE POINTER BLOCK                        
         SPACE 1                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PRTD     DSECT                                                                  
         DS    CL20                SPARE                                        
PRTLBOX  DS    C                   LEFT HAND BOX                                
PRTOFF   DS    CL2                 OFFICE                                       
PRTBOX1  DS    C                                                                
PRTNAME  DS    CL36                OFFICE NAME                                  
PRTBOX2  DS    C                                                                
         DS    CL1                                                              
PRTOG    DS    C                   OFFICE GROUP                                 
         DS    CL1                                                              
PRTBOX3  DS    C                                                                
PRTDCLM  DS    CL6                 DRAFT CLAIM NUMBER                           
PRTBOX4  DS    C                                                                
PRTLCLM  DS    CL6                 LIVE CLAIM NUMBER                            
PRTRBOX  DS    C                                                                
         SPACE 2                                                                
* ACGENFILE                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACPRO0C   12/03/09'                                      
         END                                                                    
