*          DATA SET ACPRO60    AT LEVEL 032 AS OF 08/10/00                      
*PHASE T60B60A                                                                  
         TITLE 'ACCOUNT GROUP DISPLAY/CHANGE'                                   
T60B60   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T60B60**,RR=R2,CLEAR=YES                            
         LR    R7,RC                                                            
         USING MYD,R7              LOCAL W/S                                    
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC             GENERAL W/S                                  
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   MODE04                                                           
         LA    R2,AGPMTYH                                                       
         MVC   KEY,SPACES                                                       
         L     R6,AIO              R6=A(RECORD FOR DELETION)                    
         L     R3,AIO2             READ ACCDIR FOR PASSIVE RECORDS              
         USING AGPRECD,R3                                                       
         MVC   AGPKEY(L'AGPKEY),0(R6)                                           
         MVI   AGPKTYP,AGPKTYPQ                                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCDIR',AGPKEY,AGPKEY                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   AGPKTYP,AGPKTYPQ                                                 
         BNE   MODE02                                                           
         CLC   AGPKCPY(AGPKULA-AGPKCPY),1(R6)                                   
         BNE   MODE02                                                           
         MVI   ERROR,CANTDEL       A/C GROUP ATTACHED TO A CLIENT               
         B     ERREND                                                           
MODE02   GOTO1 PERSIN                                                           
         BAS   RE,DREC                                                          
         B     OKEXIT                                                           
*                                                                               
MODE04   CLI   MODE,RECREST                                                     
         BNE   MODE06                                                           
         GOTO1 PERSIN                                                           
         BAS   RE,DREC                                                          
         B     OKEXIT                                                           
*                                                                               
MODE06   GOTO1 DICTATE,DMCB,C'LL  ',DDIN,DDOUT                                  
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE08                                                           
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE08   CLI   MODE,VALREC                                                      
         BNE   MODE10                                                           
         BAS   RE,VREC                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE10   CLI   MODE,DISPKEY                                                     
         BNE   MODE12                                                           
         BAS   RE,DKEY                                                          
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE12   CLI   MODE,DISPREC                                                     
         BNE   XIT                                                              
         BAS   RE,DREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              VALIDATE KEY ACCOUNT GROUP RECORD                    *           
*********************************************************************           
         SPACE                                                                  
VKEY     NTR1                                                                   
         BAS   RE,SPFKEY           SET PF KEY LINE                              
*                                                                               
         GOTO1 SETCOMP                                                          
         GOTO1 SETHEIR                                                          
         MVC   XTYPE,SPACES                                                     
         MVC   HTYPE,SPACES                                                     
         MVC   XCODE,SPACES                                                     
*                                                                               
         LA    R2,AGPMTYH          ACCOUNT GROUP TYPE                           
         USING FLDHDRD,R2                                                       
         GOTO1 ANY                 MUST HAVE INPUT                              
         MVI   ERROR,INVALID                                                    
         CLI   AGPMTY,C'1'         MUST NOT BE LESS THAN 1                      
         BL    ERREND                                                           
         CLI   AGPMTY,C'2'         OR GREATER THAN 2                            
         BH    ERREND                                                           
         MVC   XTYPE,AGPMTY                                                     
         MVC   HTYPE,AGPMTY                                                     
         NI    HTYPE,X'FF'-X'F0'                                                
         OI    FLDOIND,FOUTTRN                                                  
         CLI   CALLSP,0            ARE WE GOING BACK TO LIST?                   
         BE    *+8                                                              
         OI    FLDATB,FATBPROT     YES - PROTECT THIS FIELD                     
*                                                                               
         LA    R2,AGPMCOH          ACCOUNT GROUP CODE                           
         GOTO1 ANY                 MUST HAVE INPUT                              
         LA    RE,AGPMCO                                                        
         CLC   =C'ALL',0(RE)       'ALL' IS USED BY REQUEST PROGRAM             
         BE    ERREND                                                           
         XR    RF,RF                                                            
         IC    RF,FLDILEN                                                       
         CH    RF,=H'3'            MUST BE 3 CHARACTERS LONG                    
         BE    VKEY02                                                           
         MVI   ERROR,AGSHTLEN                                                   
         B     ERREND                                                           
VKEY02   CLI   0(RE),C'A'          EACH CHARACTER MUST BE ALPHA                 
         BL    ERREND                                                           
         CLI   0(RE),C'9'          OR NUMERIC                                   
         BH    ERREND                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,VKEY02                                                        
         IC    RF,FLDILEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   XCODE(0),AGPMCO                                                  
         OI    FLDOIND,FOUTTRN                                                  
         CLI   CALLSP,0            ARE WE GOING BACK TO LIST?                   
         BE    *+8                                                              
         OI    FLDATB,FATBPROT     YES - PROTECT THIS FIELD                     
*                                                                               
         LA    R3,KEY              PRESET KEY FOR GENCON                        
         USING AGRRECD,R3                                                       
         XC    AGRKEY,AGRKEY                                                    
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKCPY,CUL                                                      
         MVC   AGRKGTYP,XTYPE                                                   
         MVC   AGRKAGR,XCODE                                                    
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         CLI   ACTNUM,ACTNADD                                                   
         BNE   OKEXIT                                                           
         MVC   AIO,AIO2            SWAP IO AREA                                 
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                CHECK RECORD ALREADY EXISTS                  
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(L'AGRKEY),KEYSAVE                                            
         BNE   VKEY04                                                           
         MVI   ERROR,RECXIST       RECORD ALREADY EXISTS                        
         TM    DMCB+8,X'02'                                                     
         BNO   ERREND                                                           
         MVI   ERROR,MUSTREST      RECORD MUST BE RESTORED                      
         B     ERREND                                                           
VKEY04   MVC   AIO,AIO1            RESTORE IO AREA                              
         MVC   KEY,SAVEKEY         RESET KEY                                    
         B     OKEXIT                                                           
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
*              DISPLAY KEY FOR ACCOUNT GROUP RECORD                 *           
*********************************************************************           
         SPACE                                                                  
DKEY     NTR1                                                                   
         L     R3,AIO              ONLY CALLED FOR ACTION SELECT                
         USING AGRRECD,R3                                                       
         MVC   AGPMTY,AGRKGTYP                                                  
         OI    AGPMTYH+(FLDOIND-FLDHDRD),FOUTTRN                                
         MVC   AGPMCO,AGRKAGR                                                   
         OI    AGPMCOH+(FLDOIND-FLDHDRD),FOUTTRN                                
         MVI   HEADSET,C'N'                                                     
         MVC   KEY,SAVEKEY                                                      
         B     OKEXIT                                                           
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
*              DISPLAY ACCOUNT GROUP RECORD                         *           
*********************************************************************           
         SPACE 1                                                                
DREC     NTR1                                                                   
         TWAXC AGPMCONH            CLEAR ALL UNPROTECTED FIELDS                 
         XC    AGPMOFN,AGPMOFN     AND OFFICE NAME FIELD                        
         OI    AGPMOFNH+(FLDOIND-FLDHDRD),FOUTTRN                               
*                                                                               
         L     R6,AIO                                                           
         AH    R6,DATADISP                                                      
         USING NAMELD,R6                                                        
DREC06   CLI   NAMEL,NAMELQ                                                     
         BNE   DREC08                                                           
         LA    R2,AGPMCONH                                                      
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   L'AGPMCONH(0,R2),NAMEREC                                         
         OI    FLDOIND,FOUTTRN                                                  
         B     DREC10                                                           
*                                                                               
         USING RSTELD,R6                                                        
DREC08   CLI   RSTEL,RSTELQ                                                     
         BNE   DREC10                                                           
         MVC   AGPMOF,RSTOFFC                                                   
         B     DREC10                                                           
*                                                                               
DREC10   XR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   DREC06                                                           
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         USING CPYRECD,R4                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CUL                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'CPYKEY),KEYSAVE                                            
         BNE   DREC16                                                           
         L     R4,AIO                                                           
         AH    R4,DATADISP                                                      
         USING FFTELD,R4                                                        
DREC12   CLI   FFTTYPE,FFTTAAGR                                                 
         BNE   DREC14                                                           
         CLC   FFTSEQ,HTYPE                                                     
         BNE   DREC14                                                           
         XR    RF,RF                                                            
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   AGPMTYN(0),FFTDATA                                               
         OI    AGPMTYNH+(FLDOIND-FLDHDRD),FOUTTRN                               
         B     DREC16                                                           
DREC14   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   DREC12                                                           
         DROP  R4                                                               
*                                                                               
DREC16   CLC   AGPMOF,SPACES                                                    
         BNH   DREC18                                                           
         LA    R2,AGPMOFH                                                       
         LA    R4,KEY                                                           
         USING OGRRECD,R4                                                       
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY(3),CUL                                                   
         MVC   OGRKOFC,8(R2)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'OGRKEY),KEYSAVE                                            
         BNE   DREC18                                                           
         LA    R2,AGPMOFNH                                                      
         GOTO1 NAMEOUT                                                          
DREC18   MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 PERSOUT             SHOW ACTIVITY DETAILS                        
         LA    R2,AGPMACTH                                                      
         MVC   8(20,R2),WORK+20                                                 
         OI    FLDOIND,FOUTTRN                                                  
*                                                                               
         CLI   CALLSP,0            ARE WE GOING BACK TO LIST?                   
         BE    DRECX                                                            
         LA    R2,AGPMCONH         YES - SET CURSOR TO CODE NAME FIELD          
         OI    FLDOIND,FOUTCUR                                                  
         CLI   HEADSET,C'Y'        HAS HEADLINE ALREADY BEEN SET?               
         BE    DRECX                                                            
         BAS   RE,SHEADER          NO - FORMAT HEADLINE MESSAGE                 
         MVI   ERROR,X'FE'                                                      
         B     ERREND                                                           
*                                                                               
DRECX    MVC   KEY,SAVEKEY                                                      
         B     OKEXIT                                                           
         EJECT                                                                  
*********************************************************************           
*              VALIDATE ACCOUNT GROUP RECORD                        *           
*********************************************************************           
         SPACE 1                                                                
VREC     NTR1                                                                   
         MVI   OPTION,C'Y'         SET OPTION TO DISPLAY NAME                   
*                                                                               
         CLI   ACTNUM,ACTNADD                                                   
         BE    VREC04                                                           
         LA    R2,AGPMCONH                                                      
VREC02   TM    FLDIIND,FINPTHIS    ANY FIELDS INPUT THIS TIME?                  
         BO    VREC04              YES                                          
         BAS   RE,BUMPTOUN                                                      
         CLI   0(R2),0                                                          
         BNE   VREC02                                                           
         B     VRECX               NO                                           
*                                                                               
VREC04   LA    R2,AGPMCONH         REPLACE NAME ELEMENT                         
         MVI   ELCODE,NAMELQ                                                    
         GOTO1 REMELEM                                                          
         GOTO1 ANY                                                              
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING NAMELD,R6                                                        
         MVI   NAMEL,NAMELQ                                                     
         XR    RF,RF                                                            
         IC    RF,AGPMCONH+5                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   NAMEREC(0),AGPMCON                                               
         LA    RF,NAMLN1Q+1(RF)                                                 
         STC   RF,NAMLN                                                         
         GOTO1 ADDELEM                                                          
         OI    FLDIIND,FINPVAL                                                  
*                                                                               
VREC06   LA    R2,AGPMOFH          VALIDATE ANY NEW OFFICE                      
         CLI   FLDILEN,0                                                        
         BE    VREC08                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 VALOFF                                                           
         MVC   AGPMOF,WORK                                                      
         MVC   AIO,AIO1                                                         
         B     VREC10                                                           
VREC08   MVC   AGPMOF,SPACES                                                    
         XC    AGPMOFN,AGPMOFN                                                  
         OI    AGPMOFNH+6,X'80'                                                 
*                                                                               
VREC10   MVI   ELCODE,RSTELQ       REPLACE STATUS ELEMENT                       
         GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING RSTELD,R6                                                        
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN2Q                                                    
         MVC   RSTOFFC,AGPMOF                                                   
         GOTO1 ADDELEM                                                          
         OI    FLDIIND,FINPVAL                                                  
*                                                                               
VREC12   GOTO1 PERSIN                                                           
VRECX    MVC   KEY,SAVEKEY                                                      
         B     OKEXIT                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*              SET PFKEY LINE                                         *         
***********************************************************************         
         SPACE 1                                                                
SPFKEY   NTR1                                                                   
         LA    RE,AGPMPFH                                                       
         LA    RF,AS$PFRET                                                      
         CLI   CALLSP,0            ARE WE GOING BACK TO LIST?                   
         BNE   SPFK02              YES                                          
         B     SPFK04              (NO PFKEYS DEFINED)                          
*                                                                               
SPFK02   XC    WORK,WORK                                                        
         GOTO1 GETTXT,WORK,(RF),('PFLMAX',(RE)),(C'S',0)                        
         B     SPFK06                                                           
*                                                                               
SPFK04   MVC   8(L'AGPMPF,RE),SPACES                                            
*                                                                               
SPFK06   OI    AGPMPFH+6,X'80'                                                  
         XIT1                                                                   
         SPACE 1                                                                
***********************************************************************         
*              SET HEADER LINE                                        *         
***********************************************************************         
         SPACE 1                                                                
SHEADER  NTR1                                                                   
         LA    RF,AI$RDECH         'RECORD DISPLAYED - ENTER CHANGES'           
         CLI   MODE,DISPREC                                                     
         BNE   *+8                                                              
         LA    RF,AI$RECDS         'RECORD DISPLAYED'                           
         XR    RE,RE                                                            
         ICM   RE,4,GETMSYS                                                     
         XC    WORK,WORK                                                        
         GOTO1 GETTXT,WORK,(RF),(0,CONHEADH),(C'I',0),0,0,(RE)                  
         OI    CONHEADH+6,X'80'                                                 
         MVI   HEADSET,C'Y'                                                     
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              OTHER ROUTINES                                         *         
***********************************************************************         
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         BR    RE                                                               
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
*                                                                               
OKEXIT   CR    RE,RE                                                            
XIT      XIT1                                                                   
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
         SPACE 1                                                                
*********************************************************************           
*              DICTIONARY                                           *           
*********************************************************************           
         SPACE 1                                                                
DDIN     DS    0C                                                               
         DCDDL AC#YES,3                                                         
         DCDDL AC#NO,2                                                          
         DC    X'00'                                                            
         SPACE 1                                                                
*********************************************************************           
*              LITERAL POOL                                         *           
*********************************************************************           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*              WORKING STORAGE                                      *           
*********************************************************************           
         SPACE 1                                                                
MYD      DSECT                                                                  
         DS    0F                                                               
RELO     DS    A                                                                
         SPACE 1                                                                
DDOUT    DS    0C                                                               
         DSDDL PRINT=YES                                                        
         SPACE 1                                                                
* ACGOBLOCK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
MYDEND   EQU   *                                                                
         EJECT                                                                  
*********************************************************************           
*              INCLUDED DSECT BOOKS ETC                             *           
*********************************************************************           
         SPACE 1                                                                
* ACPROWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* ACMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACMSGEQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACPROAED                                                       
         ORG   AGPWORK                                                          
XCODE    DS    XL3                                                              
XTYPE    DS    XL1                                                              
HTYPE    DS    XL1                                                              
HEADSET  DS    CL1                                                              
SAVEKEY  DS    XL42                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACPRO60   08/10/00'                                      
         END                                                                    
