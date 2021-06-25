*          DATA SET ACCAP06    AT LEVEL 013 AS OF 12/11/09                      
*PHASE T61D06A                                                                  
*                                                                               
*INCLUDE QSORT                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        ACCAP06 -- METHOD MAINTENANCE/LIST                   *         
*                                                                     *         
*  COMMENTS:     MAINTAINS METHOD RECORDS                             *         
*                                                                     *         
*  CALLED FROM:  CAP CONTROLLER (T61D00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS ACCAPF7 (MAINTENANCE)                        *         
*                        ACCAPF6 (LIST)                               *         
*                                                                     *         
*  OUTPUTS:      UPDATED METHOD RECORDS, LIST                         *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
* EMOU 009 18AUG00 PASS LIMIT ACCESS OFFICE TO BMONVAL                          
* NSHE 011 12OCT04 INCREASE SIZE OF ALOCHIST TABLE TO ENSURE LATEST             
*                  ALLOCATION IS SHOWN                                          
* NSHE 013 29OCT04 ALLOW TO SCROLL ALLOCATION HISTORY                           
         TITLE 'T61D06 - METHOD RECORD MAINT/LIST'                              
T61D06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1D06**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING T61DFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                                                      
         USING BLOCKSD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         OI    GENSTAT2,RETEQSEL   SET FOR PAGING                               
         SR    R2,R2               SET PFKEY TABLE ADDR                         
         LA    R3,MEMPFKYH                                                      
         CLI   PFKEY,7                                                          
         BE    INIT10                                                           
         CLI   PFKEY,8                                                          
         BE    INIT10                                                           
         LA    R2,PFTABLE                                                       
INIT10   CLI   ACTEQU,ACTLIST                                                   
         BNE   PFINIT                                                           
         LA    R2,LPFTABLE                                                      
         LA    R3,MELPFKYH                                                      
*                                                                               
*              PROGRAM INITIALIZATION - DON'T CLEAR WORKING STORAGE             
PFINIT   GOTO1 INITIAL,DMCB,(X'40',(R2)),(R3)                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER ADD                                    
         BE    XA                                                               
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    XP                                                               
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                           
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         XC    METHCODE,METHCODE   METHOD CODE                                  
         XC    METHNUM,METHNUM                                                  
*                                                                               
         LA    R2,CONOPTH          NO VALID OPTIONS                             
         CLI   5(R2),0                                                          
         BNE   EINVOPT                                                          
*                                                                               
         CLI   ACTEQU,ACTLIST                                                   
         BE    VK105                                                            
*                                                                               
VK10     LA    R2,MEMCODEH                                                      
         CLI   MEMCODEH+5,0        ANY DATA?                                    
         BE    ERRPLS              MISSING DATA                                 
*                                                                               
         TM    4(R2),X'08'         VALID NUMERIC                                
         BNO   VK20                                                             
*                                                                               
*        READ RECORD BY METHOD NUMBER                                           
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    EINVMETH            NEED CODE ON ADD                             
*                                                                               
         CLI   5(R2),1             ONLY 1-9                                     
         BNE   EINVMETH                                                         
         MVC   METHNUM,8(R2)                                                    
*                                                                               
         LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CAHRECD,R6                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE BY NUM         
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM    METHOD NUMBER                                
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   EINVMETH                                                         
         MVC   AIO,AIO3            METHOD RECORD IN AIO3                        
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     VK50                                                             
         DROP  R6                                                               
*                                                                               
*        READ RECORD BY METHOD CODE                                             
*                                                                               
VK20     MVC   METHCODE,MEMCODE    SAVE METHOD CODE                             
         OC    METHCODE,SPACES                                                  
*                                                                               
         CLI   ACTEQU,ACTADD       ARE WE ADDING A NEW RECORD                   
         BNE   VK30                NO                                           
         XC    SVSCRADD,SVSCRADD   YES - CLEAR ADDRESS FIELD FOR SCROLL         
         B     VK100               SKIP VALIDATION ON ADD                       
*                                                                               
VK30     LA    R6,BIGKEY           VALIDATE METHOD                              
         USING CMTRECD,R6                                                       
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CMTKSUB,CMTKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CMTKCPY,CMPY        COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD CODE                                  
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BNE   EINVMETH                                                         
         MVC   AIO,AIO3            METHOD RECORD IN AIO3                        
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     VK50                                                             
*                                                                               
*        GET BOTH METHNUM AND METHCODE                                          
*                                                                               
VK50     DS    0H                                                               
         USING METELD,R6                                                        
         L     R6,AIO3                                                          
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         MVC   METHNUM,METNUM      SAVE BOTH NUMBER AND CODE                    
         MVC   METHCODE,METCODE                                                 
         CLC   SVMETCDE,METCODE    IS METHOD CODE SAME                          
         BE    VK100               YES                                          
         XC    SVSCRADD,SVSCRADD   NO - CLEAR ADDRESS FIELD FOR SCROLL          
         MVC   SVMETCDE,METCODE                                                 
*                                                                               
VK100    OC    METHNUM,METHNUM     DO WE NEED A NEW NUMBER                      
         BNZ   VK105                                                            
         BAS   RE,CALCNUM                                                       
         BNE   EMETH9                                                           
*                                                                               
VK105    DS    0H                                                               
*&&US                                                                           
         CLI   ACTEQU,ACTADD       IF ADDING A NEW METHOD                       
         BNE   *+8                                                              
         BRAS  RE,VALDEF           VALIDATE THAT THE PROPER DEFAULT             
*                                  14 AND 15 COST ACCTS EXIST                   
*&&                                                                             
         LA    R4,BIGKEY           ALWAYS BUILD KEY BY CODE NOT NUM             
         USING CMTRECD,R4                                                       
*                                                                               
         MVC   CMTKEY,SPACES                                                    
         MVI   CMTKTYP,CMTKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CMTKSUB,CMTKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CMTKCPY,CMPY        COMPANY                                      
         MVC   CMTKMTHD,METHCODE   METHOD CODE                                  
         MVC   SAVEKEY,BIGKEY                                                   
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RECORD                                                        
***********************************************************************         
*                                                                               
         USING CMTRECD,R4                                                       
VR       L     R4,AIO              BUILD RECORD                                 
         MVC   METHCODE,CMTKMTHD   SAVE METHOD CODE                             
*                                                                               
         USING METELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   VR05                                                             
         MVC   METHNUM,METNUM      SAVE METHOD NUMBER                           
*                                                                               
VR05     OC    METHNUM,METHNUM                                                  
         BNZ   VR10                                                             
         BAS   RE,CALCNUM          GET NEXT NUMBER ON ADD                       
*                                                                               
VR10     MVI   ELCODE,NAMELQ       NAME ELEMENT                                 
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING NAMELD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ                                                     
         LA    R2,MEMDESCH                                                      
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         AH    R1,=Y(NAMLN1Q)                                                   
         STC   R1,NAMLN                                                         
         MVC   NAMEREC,WORK         ACTUAL DESCRIPTION                          
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,METELQ       MATHOD OF ALLOCATION ELEMENT                 
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING METELD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   METEL,METELQ                                                     
         MVI   METLN,METLNQ                                                     
*                                                                               
         MVC   METCODE,METHCODE                                                 
         MVC   METNUM,METHNUM                                                   
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR20     B     DR                  DISPLAY THE RECORD                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THAT THE DEFAULT COST PROFILE ACCOUNT EXIST WHEN ADDING A  *         
* NEW METHOD                                                          *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
         USING DEFTABD,R4                                                       
         USING ACTRECD,R6                                                       
VALDEF   NTR1                                                                   
*                                                                               
         LA    R4,DEFTAB           POINT TO TABLE OF DEFAULTS                   
VDEF10   CLI   0(R4),X'FF'                                                      
         BE    VDEFX                                                            
*                                                                               
         LA    R6,BIGKEY                                                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(L'DEFUL),DEFUL    UNIT AND LEDGER FROM TABLE             
         MVC   ACTKACT(L'DEFACC),DEFACC  ACCOUNT FROM TABLE                     
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    *+14                                                             
         MVC   SVDACC(L'DEFUL+L'DEFACC),DEFUL                                   
         B     ERRVDEF                                                          
*                                                                               
         LA    R4,DEFLNQ(R4)       BUMP TO NEXT TABLE ENTRY                     
         B     VDEF10                                                           
*                                                                               
VDEFX    B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
*        CALCULATE THE NEXT METHOD NUMBER ON ACTION ADD                         
***********************************************************************         
*                                                                               
CALCNUM  NTR1                                                                   
         LA    R4,KEY2             BUILD KEY                                    
         USING CAHRECD,R4                                                       
*                                                                               
         XC    KEY2,KEY2           GET FIRST ONE                                
         MVI   CAHKTYP,CAHKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CAHKSUB,CAHKSUBQ    METHOD BY NUMBER SUB TYPE                    
         MVC   CAHKCPY,CMPY        COMPANY                                      
*                                                                               
         LA    R2,1                ASSUME FIRST                                 
         MVC   KEYSAVE,KEY2                                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI ',=C'ACCDIR',KEY2,KEY2                    
         B     CN10                                                             
*                                                                               
CNSEQ    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCDIR ',KEY2,KEY2                    
CN10     CLC   KEY2(3),KEYSAVE                                                  
         BNE   CN20                                                             
         PACK  DUB,CAHKMTHD                                                     
         CVB   R2,DUB                                                           
         LA    R2,1(R2)            WANT NEXT NUMBER                             
         B     CNSEQ               MIGHT NOT BE THE LAST NUMBER USED            
*                                                                               
CN20     CH    R2,=H'10'           CAN ONLY GO UP TO 10                         
         BNL   XNO                                                              
         EDIT  (R2),(1,METHNUM)                                                 
CNX      B     XYES                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY KEY                                                            
***********************************************************************         
*                                                                               
DK       DS    0H                                                               
         LA    R4,BIGKEY           ALWAYS BUILD KEY BY CODE NOT NUM             
         USING CMTRECD,R4                                                       
*                                                                               
         MVC   MEMCODE,CMTKMTHD                                                 
         OI    MEMCODEH+6,X'80'                                                 
         CLC   SVMETCDE,MEMCODE    IS METHOD CODE SAME                          
         BE    DKX                 YES                                          
         XC    SVSCRADD,SVSCRADD   NO - CLEAR ADDRESS FIELD FOR SCROLL          
         MVC   SVMETCDE,MEMCODE                                                 
*                                                                               
DKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY RECORD                                                         
***********************************************************************         
*                                                                               
DR       DS    0H                                                               
         MVC   MEMDESC,SPACES      CLEAR DESCRIPTION                            
*                                                                               
         LA    R2,MEMCOL1H         CLEAR METHOD HISTORY DISPLAY                 
         LA    R3,MEMENDH                                                       
DRCLR    ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
DRCLR5   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    DRCLR               NO                                           
*                                                                               
         USING CMTRECD,R6                                                       
         L     R6,AIO                                                           
         MVC   MEMCODE,CMTKMTHD    DISPLAY CODE                                 
         OI    MEMCODEH+6,X'80'    TRANSMIT                                     
*                                                                               
         MVI   ELCODE,NAMELQ       DESCRIPTION                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING NAMELD,R6                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'            2 FOR CODE AND LEN, 1 FOR EX                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MEMDESC(0),NAMEREC                                               
         OI    MEMDESCH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING METELD,R6                                                        
         MVC   MEMNUM,METNUM       DISP NUMBER                                  
         OI    MEMNUMH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
*        BUILD TABLE OF ELEMENTS AND SORT ON DATE (RECENT FIRST)                
*                                                                               
         USING TABLED,R4                                                        
         LA    R4,ALOCHIST         BUILD ALLOCATION HISTORY                     
         XC    TABCOUNT,TABCOUNT                                                
*                                                                               
         LA    R0,ALOCHIST         CLEAR BLOCK                                  
         L     R1,=A(ALOCEND-ALOCHIST)                                          
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING DOAELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,DOAELQ       DATE OF ALLOCATION ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   DR32                                                             
         B     DR30                                                             
*                                                                               
DR30NX   BAS   RE,NEXTEL                                                        
         BNE   DR32                                                             
*                                                                               
DR30     DS    0H                                                               
         MVC   TABMONTH,DOAYR                                                   
         MVC   TABDATE,DOADTE                                                   
*                                                                               
         LH    R1,TABCOUNT         COUNT NUMBER OF ENTRIES FOR SORT             
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
*                                                                               
         LA    R4,TABLENQ(R4)                                                   
         LA    R1,ALOCEND                                                       
         CR    R4,R1                                                            
         BL    DR30NX                                                           
*                                                                               
DR32     DS    0H                                                               
         LH    R2,TABCOUNT                                                      
         L     RF,=V(QSORT)                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(C'N',ALOCHIST),(R2),TABLENQ,L'TABMONTH,      X        
               TABMONTH-TABLED                                                  
*                                                                               
         XC    TABCOUNT,TABCOUNT                                                
         LA    R4,ALOCHIST                                                      
DR34     LH    R1,TABCOUNT         COUNT NUMBER OF ENTRIES FOR SORT             
         LA    R1,1(R1)                                                         
         STH   R1,TABCOUNT                                                      
         STH   R1,TABNUM                                                        
         LA    R4,TABLENQ(R4)                                                   
         OC    TABMONTH,TABMONTH                                                
         BNZ   DR34                                                             
         LA    R4,ALOCHIST                                                      
*                                                                               
         OC    SVSCRADD,SVSCRADD   IS IT FIRST TIME FOR THIS RECORD             
         BNZ   DR40                NO                                           
         LA    R2,SCRLLST          YES - POINT R5 TO START OF SCROLL            
         B     DR54                                                             
*                                                                               
DR40     L     R2,SVSCRADD                                                      
         CLI   PFKEY,0                                                          
         BE    DR44                                                             
         CLI   PFKEY,7             SCROLL DOWN                                  
         BNE   DR42                NO                                           
         CLI   1(R2),1             ARE WE ALREADY AT BEGINNING                  
         BE    DR44                YES - JUST REDISPLAY                         
         SHI   R2,2                NO - GET PREV ENTRY IN SCROLL TAB            
DR42     CLI   PFKEY,8                                                          
         BNE   DR44                                                             
         CLI   1(R2),X'FF'         HAVE WE GOT TO END OF THE SCROLL             
         BE    DR44                YES - JUST REDISPLAY LAST SCREEN             
         AHI   R2,2                BUMP TO NEXT ENTRY IN SCOLL TAB              
         CLC   TABCOUNT,0(R2)      CHECK WE HAVE MORE TO DISPLAY                
         BNL   DR44                YES                                          
         SHI   R2,2                NO - REDISPLAY LAST SCREEN                   
                                                                                
DR44     CLC   TABNUM,0(R2)                                                     
         BE    DR54                                                             
         LA    R4,TABLENQ(R4)                                                   
         OC    0(TABLENQ,R4),0(R4) ANY ENTRY                                    
         BNZ   DR44                                                             
*                                                                               
DR54     DS    0H                  DISPLAY ELEMENTS                             
         ST    R2,SVSCRADD                                                      
         LA    R2,MEMCOL1H         COLUMN 1 HEADER                              
         USING COLLINED,R2         COLUMN DSECT                                 
*                                                                               
DR56     DS    0H                                                               
         BAS   RE,DISPALOC         DISPLAY ALLOCATION                           
         BE    *+12                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         B     DRX                                                              
         BAS   RE,BUMP3            NEXT IN COLUMN                               
         LA    R1,MEMEND1H                                                      
         CR    R2,R1                                                            
         BNH   DR56                                                             
*                                                                               
         LA    R2,MEMCOL2H         COLUMN 2 HEADER                              
DR58     BAS   RE,DISPALOC         DISPLAY ALLOCATION                           
         BE    *+12                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         B     DRX                                                              
         BAS   RE,BUMP3            NEXT IN COLUM                                
         LA    R1,MEMEND2H                                                      
         CR    R2,R1                                                            
         BNH   DR58                                                             
*                                                                               
         LA    R2,MEMCOL3H         COLUMN 3 HEADER                              
DR60     BAS   RE,DISPALOC         DISPLAY ALLOCATION                           
         BE    *+12                                                             
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         B     DRX                                                              
         BAS   RE,BUMP3            NEXT IN COLUM                                
         LA    R1,MEMEND3H                                                      
         CR    R2,R1                                                            
         BNH   DR60                                                             
*                                                                               
DRX      B     XIT                                                              
*                                                                               
*        DISPLAY ALLOCATION INFORMATION AND BUMP TO NEXT IN TABLE               
*                                                                               
DISPALOC NTR1                                                                   
         OC    0(TABLENQ,R4),0(R4) ANY ENTRY                                    
         BZ    DAXNO                                                            
         XC    MONYR,MONYR                                                      
         MVC   MONYR(2),TABMONTH                                                
         GOTO1 DATCON,DMCB,(1,MONYR),(9,COLMONTH)                               
         GOTO1 DATCON,DMCB,(2,TABDATE),(10,COLDATE)                             
*                                                                               
         XC    MONYR,MONYR                                                      
         MVC   MONYR(2),TABMONTH                                                
         MVI   MONYR+2,X'01'                                                    
         GOTO1 DATCON,DMCB,(1,MONYR),(10,WORK)                                  
*                                                                               
*&&US*&& GOTO1 VBMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,BLOCK),(CMPY,0)          
*&&UK                                                                           
         GOTO1 VBMONVAL,DMCB,(8,WORK),(97,ACOMFACS),(0,BLOCK),         X        
               (CMPY,TWAACCS)                                                   
*&&                                                                             
         LA    R1,BLOCK                                                         
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ      EVERYTHING OK?                               
         BE    *+8                                                              
         MVI   COLLOCK,C'*'        FLAG LOCKED MONTHS                           
         DROP  R1                                                               
*                                                                               
         LA    R4,TABLENQ(R4)                                                   
         OC    0(TABLENQ,R4),0(R4) ANY ENTRY                                    
         BZ    DAXNO                                                            
         LA    R1,ALOCEND                                                       
         CR    R4,R1                                                            
         BL    DAXYES                                                           
         B     DAXNO                                                            
*                                                                               
DAXYES   SR    RC,RC                                                            
DAXNO    LTR   RC,RC                                                            
         XIT1  REGS=(R4)                                                        
*                                                                               
*        BUMP 3 FIELDS TO NEXT IN COLUMN                                        
*                                                                               
BUMP3    NTR1                                                                   
         LA    R4,3                                                             
BUMPLP   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R4,BUMPLP                                                        
         XIT1  REGS=(R2)           RETURN R2                                    
*                                                                               
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        ON-SCREEN LIST                                                         
***********************************************************************         
*                                                                               
LR       DS    0H                                                               
         XC    SVSCRADD,SVSCRADD   CLEAR ADDRESS FIELD FOR SCROLL               
         LA    R4,BIGKEY                                                        
         USING CMTRECD,R4                                                       
         LA    R2,LISTAR           LINE DSECT                                   
         USING LSTLINED,R2                                                      
*                                                                               
         OC    BIGKEY,BIGKEY       FIRST TIME THROUGH?                          
         BNZ   LRHI                                                             
*                                                                               
         MVC   CMTKEY,SPACES          LIST ALL METHOD TYPES                     
         MVI   CMTKTYP,CMTKTYPQ    ALLOCATION METHOD RECORD TYPE                
         MVI   CMTKSUB,CMTKSUBQ    METHOD BY CODE SUB TYPE                      
         MVC   CMTKCPY,CMPY        COMPANY                                      
*                                                                               
LRHI     DS    0H                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LRSEQ    DS    0H                                                               
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     GOTO1 GETREC                                                           
         CLC   BIGKEY(3),SAVEKEY   SAME RECORD TYPE AND COMPANY?                
         BNE   LRX                 NO MORE DATATYPES TO LIST                    
*                                                                               
         L     R4,AIO                                                           
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSTCODE,CMTKMTHD    PUT METHOD CODE IN LIST LINE                 
*                                                                               
         USING NAMELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,NAMELQ       DESCRIPTION                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'            2 FOR CODE AND LEN, 1 FOR EX                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTDESC(0),NAMEREC                                               
         DROP  R6                                                               
*                                                                               
         USING METELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,METELQ       METHOD OF ALLOCATION ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         MVC   LSTNUM,METNUM                                                    
         DROP  R6                                                               
*                                                                               
         XC    ALLCDATE,ALLCDATE                                                
         XC    MONYR,MONYR                                                      
         USING DOAELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,DOAELQ       DATE OF ALLOCATION ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   LR55                                                             
         B     LR50                                                             
*                                                                               
LR50NX   BAS   RE,NEXTEL                                                        
         BNE   LR55                                                             
*                                                                               
LR50     DS    0H                                                               
         MVC   MONYR(1),DOAYR      SAVE VALUES                                  
         MVC   MONYR+1(1),DOAMON                                                
         MVC   ALLCDATE,DOADTE                                                  
         B     LR50NX                                                           
*                                                                               
LR55     DS    0H                  DISPLAY MOST RECENT DATES IF ANY             
         OC    MONYR,MONYR                                                      
         BZ    LR60                                                             
         GOTO1 DATCON,DMCB,(1,MONYR),(9,LSTMONTH)                               
         GOTO1 DATCON,DMCB,(2,ALLCDATE),(10,LSTDATE)                            
*                                                                               
LR60     DS    0H                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LRSEQ               NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         DROP  R6,R2,R4                                                         
         EJECT                                                                  
***********************************************************************         
*        ADD SECOND RECORD                                                      
***********************************************************************         
*                                                                               
XA       DS    0H                                                               
         L     R4,AIO              BUILD RECORD                                 
         USING CAHRECD,R4                                                       
         MVI   CAHKTYP,CAHKTYPQ                                                 
         MVI   CAHKSUB,CAHKSUBQ    COST ALLOCATION HISTORY RECORD               
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM                                                 
         XC    CAHKOFC,CAHKOFC                                                  
*                                                                               
         GOTO1 ADDREC                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        CHANGE SECOND RECORD                                                   
***********************************************************************         
*                                                                               
XP       DS    0H                                                               
         LA    R4,BIGKEY           BUILD RECORD                                 
         USING CAHRECD,R4                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ                                                 
         MVI   CAHKSUB,CAHKSUBQ    COST ALLOCATION HISTORY RECORD               
         MVC   CAHKCPY,CMPY        COMPANY                                      
         MVC   CAHKMTHD,METHNUM                                                 
         XC    CAHKOFC,CAHKOFC                                                  
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
*                                                                               
XP10     MVI   ELCODE,NAMELQ       NAME ELEMENT                                 
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING NAMELD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ                                                     
         LA    R2,MEMDESCH                                                      
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         AH    R1,=Y(NAMLN1Q)                                                   
         STC   R1,NAMLN                                                         
         MVC   NAMEREC,WORK         ACTUAL DESCRIPTION                          
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,METELQ       MATHOD OF ALLOCATION ELEMENT                 
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING METELD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   METEL,METELQ                                                     
         MVI   METLN,METLNQ                                                     
*                                                                               
         MVC   METCODE,METHCODE                                                 
         MVC   METNUM,METHNUM                                                   
*                                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
         GOTO1 WRITE                                                            
         DROP  R6                                                               
*                                                                               
XP20     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        RANDOM STUFF                                                           
***********************************************************************         
*                                  GENERAL MESSAGES                             
ERRMISS  MVI   GERROR1,MISSING                                                  
         B     ERRX                                                             
ERRPLS   MVI   GERROR1,2                                                        
         MVI   GMSGTYPE,C'I'                                                    
         B     ERRX                                                             
ERRINV   MVI   GERROR1,INVALID                                                  
         B     ERRX                                                             
*                                                                               
ERRX     DS    0H                                                               
         MVI   GMSYS,X'FF'                                                      
         GOTO1 MYERR                                                            
*                                                                               
ERRVDEF  MVC   GERROR,=AL2(ACECDEF)    DEFAULT COST ACCT MISSING                
         MVI   GLTXT,L'SVDACC          APPEND THE U/L/ACC                       
         LA    R1,SVDACC                                                        
         STCM  R1,7,GATXT                                                       
         LA    R2,MEMCODEH         POINT TO METHOD CODE                         
         B     ACCERRX                                                          
*                                                                               
EMETH9   MVC   GERROR,=AL2(ACEMETH9)                                            
         B     ACCERRX                                                          
EINVOPT  MVC   GERROR,=AL2(ACEINVOP)                                            
         B     ACCERRX                                                          
EINVDATE MVC   GERROR,=AL2(ACEIVDTE)                                            
         B     ACCERRX                                                          
EINVMETH MVC   GERROR,=AL2(ACEIVMET)                                            
         B     ACCERRX                                                          
*                                                                               
ACCERRX  DS    0H                                                               
         MVI   GMSYS,6             ACC SYSTEM MESSAGES                          
         GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
*                                                                               
         LTORG                                                                  
SCRLLST  DC    AL2(1,33,65,97,129,161,193,255)                                  
         EJECT                                                                  
***********************************************************************         
*        LIST PFKEY TABLE                                                       
***********************************************************************         
*                                                                               
LPFTABLE DS    0C                                                               
*                                                                               
*        PERSON LIST                                                            
*                                                                               
         DC    AL1(LPF01X-*,01,PFTCPROG,0,0)                                    
         DCDD  AC#CPRSN,3                                                       
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
LPF01X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(LPF03X-*,03,PFTCPROG,0,0)                                    
         DCDD  AC#HIST,3                                                        
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
LPF03X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        MAINT PFKEY TABLE                                                      
***********************************************************************         
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
*        PERSON LIST                                                            
*                                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,0,0)                                    
         DCDD  AC#CPRSN,3                                                       
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
PPF01X   EQU   *                                                                
*                                                                               
*        HISTORY LIST                                                           
*                                                                               
         DC    AL1(PPF03X-*,03,PFTCPROG,0,0)                                    
         DCDD  AC#HIST,3                                                        
         DCDD  AC#HIST,8                                                        
         DCDD  AC#LIST,8                                                        
PPF03X   EQU   *                                                                
*                                                                               
*        METHOD LIST                                                            
*                                                                               
         DC    AL1(PPF10X-*,10,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#METH,8                                                        
         DCDD  AC#LIST,8                                                        
PPF10X   EQU   *                                                                
*                                                                               
*        RETURN TO CALLER                                                       
*                                                                               
         DC    AL1(PPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
PPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
*&&US                                                                           
*                                                                               
* TABLE OF LEDGERS WITH THE CORRESPONDING DEFAULT ACCT FOR THE PROFILES         
*                                                                               
DEFTAB   DC    C'14',C'Z'                                                       
         DC    C'15',C'Z'                                                       
         DC    C'15',C'Y'                                                       
         DC    C'15',C'X'                                                       
         DC    C'15',C'W'                                                       
         DC    C'15',C'V'                                                       
         DC    C'15',C'U'                                                       
         DC    C'15',C'T'                                                       
         DC    X'FF'                                                            
*                                                                               
*&&                                                                             
***********************************************************************         
*        INCLUDES                                                               
***********************************************************************         
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* ACCAPWORKD                                                                    
* ACCAPDSECT                                                                    
* ACBMONVALD                                                                    
* ACGENFILE                                                                     
* ACDDEQUS                                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE ACCAPWORKD                                                     
       ++INCLUDE ACCAPDSECT                                                     
       ++INCLUDE ACBMONVALD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
*        SCREENS                                                                
***********************************************************************         
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPF7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPF6D                                                       
         EJECT                                                                  
***********************************************************************         
*        REMAINING WORK AREA                                                    
***********************************************************************         
*                                                                               
RELO     DS    A                                                                
SVSCRADD DS    F                   SAVED SCROLL TABLE ENTRY                     
TABCOUNT DS    H                   NUMBER OF TABLE ENTRIES                      
LASTENTY DS    H                                                                
CURRENTY DS    H                   NUMBER OF TABLE ENTRIES                      
SVMETCDE DS    CL3                 METHOD CODE                                  
METHCODE DS    CL3                 METHOD CODE                                  
METHNUM  DS    CL1                 METHOD NUM (CHARACTER)                       
MONYR    DS    CL3                 SAVED LAST MONTH/YR                          
ALLCDATE DS    CL2                 SAVED LAST ALLOCATION DATE                   
SVDACC   DS    CL(DEFLNQ)          SAVED U/L/ACC FROM DEFAULT TABLE             
SAVEKEY  DS    XL70                ACCFILE KEY                                  
KEY2     DS    XL70                ACCFILE KEY                                  
SCANBLK  DS    XL32                SCANNER BLOCK                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
*                                                                               
BLOCKSD  DSECT                                                                  
*                                                                               
ALOCHIST DS    150CL(TABLENQ)                                                   
ALOCEND  DS    0H                                                               
*                                                                               
TABLED   DSECT TABLE ENTRY DSECT                                                
TABMONTH DS    CL2                 MONTH AND YEAR OF ALLOCATION                 
TABDATE  DS    CL2                 DATE ALLOCATION WAS RUN                      
TABNUM   DS    H                   NUMBER ENTRY IN TABLE                        
TABLENQ  EQU   *-TABLED                                                         
*                                                                               
COLLINED DSECT COLUMN LINE DSECT                                                
COLHEAD  DS    CL8                                                              
COLMONTH DS    CL6                                                              
COLLOCK  DS    CL1                                                              
         DS    CL7                                                              
COLDATE  DS    CL8                                                              
*                                                                               
LSTLINED DSECT                                                                  
LSTCODE  DS    CL3                 METHOD CODE                                  
         DS    CL5                                                              
LSTDESC  DS    CL36                DESCRIPTION                                  
         DS    CL3                                                              
LSTNUM   DS    CL1                                                              
         DS    CL4                                                              
LSTMONTH DS    CL6                                                              
         DS    CL4                                                              
LSTDATE  DS    CL8                                                              
*                                                                               
DEFTABD  DSECT                                                                  
DEFUL    DS    CL2                 UNIT AND LEDGER                              
DEFACC   DS    CL1                 ACCONT                                       
DEFLNQ   EQU   *-DEFTABD                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACCAP06   12/11/09'                                      
         END                                                                    
