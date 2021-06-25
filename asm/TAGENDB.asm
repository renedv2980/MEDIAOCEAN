*          DATA SET TAGENDB    AT LEVEL 012 AS OF 05/29/15                      
*PHASE T702DBE,*                                                                
         TITLE 'T702DB - COMMERCIAL VERSION MAINTENANCE'                        
T702DB   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T702DB,R7                                                 
         LR    R6,RC                                                            
         USING TMPD,R6                                                          
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         LA    RE,COVRTBL                                                       
         AHI   RE,L'COVRTBL                                                     
         ST    RE,ASVPBLK                                                       
         AHI   RE,L'SVPTRBLK                                                    
         ST    RE,AADPBLK                                                       
                                                                                
         GOTO1 INITIAL,DMCB,PFTABLE                                             
                                                                                
         BAS   RE,VK               VALIDATE KEY                                 
                                                                                
         BAS   RE,DR               DISPLAY RECORD                               
                                                                                
         BAS   RE,VR               VALIDATE RECORD                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE KEY                                  *         
***********************************************************************         
                                                                                
VK       NTR1                                                                   
         TM    VERAGYH+4,X'80'                                                  
         BO    VK10                                                             
         GOTO1 FLDVAL,DMCB,(X'40',VERAGYH),(X'80',VERCIDH)                      
         BE    XIT                                                              
                                                                                
VK10     MVI   LSTDVER,0                                                        
         GOTO1 FLDVAL,DMCB,(3,VERVR1H),VERVRLH                                  
                                                                                
         LA    R2,VERAGYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',VERAGYH),VERAGYNH                     
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'0A',VERCIDH),VERCIDNH            
                                                                                
         USING TLCOPD,R4                                                        
         LA    R4,KEY                                                           
         MVC   TGCOM,TLCOICOM      SAVE INTERNAL COMMERCIAL NUMBER              
                                                                                
         CLI   TLCOIVER,26         IF VERSION CODE IS GREATER THAN 26           
         BNH   VK20                READ MAIN COMMERCIAL RECORD                  
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'AC',0),VERCIDNH                          
         BE    VK20                                                             
         DC    H'00'                                                            
         DROP  R4                                                               
                                                                                
         USING TLCOD,R4                                                         
VK20     L     R4,AIO                                                           
         XC    SVCOKEY,SVCOKEY                                                  
         MVC   SVCOKEY(L'TLCOKEY),TLCOKEY                                       
         DROP  R4                                                               
                                                                                
         LA    R2,VERCIDH                                                       
         MVI   ELCODE,TALFELQ      COMMERCIAL CANNOT HAVE A LIFT                
         BAS   RE,GETEL                                                         
         BE    FLDINV                                                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TACOMED,TACOMEDT    MEDIA MUST BE TV                             
         BE    VK30                                                             
         CLI   TACOMED,TACOMEDR    RADIO                                        
         BE    VK30                                                             
         CLI   TACOMED,TACOMEDI    INTERNET                                     
         BE    VK30                                                             
         CLI   TACOMED,TACOMEDN    NEW MEDIA                                    
         BE    VK30                                                             
         CLI   TACOMED,TACOMEDC    OR CABLE                                     
         BNE   INVTYPE                                                          
                                                                                
VK30     TM    TGSYSTAT,TASYSMUS   IF NEW MUSIC RULES ENABLED                   
         JZ    VK40                                                             
         CLI   TACOTYPE,CTYMUS     TYPE CANNOT BE MUSIC                         
         JE    INVTYPE                                                          
                                                                                
VK40     MVC   VERCID,TACOCID      SHOW MAIN CID IN CASE VERSION INPUT          
                                                                                
         MVC   SVCOCID,TACOCID     SAVE MAIN COMMERCIAL ID                      
         MVC   SVCOMED,TACOMED     MEDIA                                        
         MVC   SVCOSEC,TACOSEC     AND LENGTH                                   
                                                                                
         USING TAFND,R4                                                         
         XC    SVWID,SVWID         SAVE WEB APPLICATION ID                      
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   VK50                                                             
         L     R4,TGELEM                                                        
         MVC   SVWID,TAFNNAME                                                   
         DROP  R4                                                               
                                                                                
VK50     GOTO1 FLDVAL,DMCB,(X'22',VERAGYH),(X'80',VERCIDH)                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DISPLAY THE VERSIONS                              *         
***********************************************************************         
                                                                                
DR       NTR1                                                                   
         CLI   MODE,DISPREC                                                     
         BE    DR10                                                             
         CLI   MODE,XRECPUT                                                     
         BNE   XIT                                                              
                                                                                
DR10     BAS   RE,BLDCOTBL                                                      
                                                                                
         GOTO1 FLDVAL,DMCB,(X'23',VERVR1H),VERVRLH                              
                                                                                
         LA    R2,VERVR1H                                                       
         LA    R3,VERVRLH                                                       
                                                                                
         USING COVRTBLD,R4                                                      
         LA    R4,COVRTBL                                                       
DR20     CLI   0(R4),X'FF'                                                      
         BE    DR50                                                             
         CLC   LSTDVER,COVRCD                                                   
         BH    DR40                                                             
                                                                                
         EDIT  COVRCD,(L'VERVR1,8(R2)),ALIGN=LEFT                               
         STC   R0,5(R2)                                                         
         OI    4(R2),X'08'                                                      
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'VERVR1N,R2),COVRID                                           
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         EDIT  COVRLN,(L'VERVR1L,8(R2)),ALIGN=LEFT                              
*  NEW 26K FIELD                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(L'COV26K,R2),COV26K                                            
*                                                                               
*                                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         EDIT  COVRIAL,(L'VERVR1A,8(R2)),ALIGN=LEFT                             
         STC   R0,5(R2)                                                         
                                                                                
DR30     ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R3                                                            
         BNH   DR40                                                             
         MVC   LSTDVER,0(R4)                                                    
         MVC   MYMSGNO,=H'261'                                                  
         B     DR60              END OF SCREEN REACHED                          
                                                                                
DR40     LA    R4,COVRLNQ(R4)                                                   
         B     DR20                                                             
         DROP  R4                                                               
                                                                                
DR50     MVI   LSTDVER,0         END OF VERSIONS REACHED                        
         MVC   MYMSGNO,=H'262'                                                  
                                                                                
DR60     LA    R2,VERAVCDH                                                      
                                                                                
         CLI   MODE,XRECPUT                                                     
         BNE   DR70                                                             
         GOTO1 FLDVAL,DMCB,(X'80',VERAVCDH),(X'80',VERDVCDH)                    
         BE    DR70                                                             
         GOTO1 FLDVAL,DMCB,(3,VERAVCDH),(X'80',VERDVCDH)                        
         XC    MYMSGNO,MYMSGNO                                                  
         B     XIT                                                              
                                                                                
DR70     MVI   MYMTYP,GTMINF                                                    
         MVI   BLOCK,0                                                          
         OI    GENSTAT2,USGETTXT                                                
         B     MSGEXIT                                                          
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE THE VERSIONS                             *         
***********************************************************************         
                                                                                
VR       NTR1                                                                   
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
                                                                                
         BAS   RE,BLDCOTBL       BUILD TABLE OF VERSIONS ON COMMERCIAL          
                                                                                
         XC    ELEMENT,ELEMENT                                                  
         MVI   SVVER,0                                                          
                                                                                
         GOTO1 FLDVAL,DMCB,(X'80',VERAVCDH),(X'80',VERDVCDH)                    
         BE    VR150                                                            
                                                                                
         GOTO1 FLDVAL,DMCB,(X'80',VERAVCDH),(X'80',VERAVALH)                    
         BE    VR110                                                            
                                                                                
         CLI   VERAVCDH+5,0      IF ADDING A VERSION ...                        
         BE    ACDMIS                                                           
         CLI   VERAVIDH+5,0      CODE, ID,                                      
         BE    AIDMIS                                                           
         CLI   VERAVLNH+5,0      AND LENGTH FIELDS MUST CONTAIN INPUT           
         BE    ALNMIS                                                           
         CLI   VERDVCDH+5,0      AND DELETE CODE FIELD MUST BE EMPTY            
         BNE   DCDINV                                                           
                                                                                
         LA    R2,VERAVCDH                                                      
         GOTO1 VALINUM           VERSION CODE MUST BE VALID NUMERIC             
         CLI   ACTUAL,1          BETWEEN 1 AND 250                              
         BL    FLDINV                                                           
         CLI   ACTUAL,250                                                       
         BH    FLDINV                                                           
                                                                                
         CLC   =C'VS',SVWID      IF COMMERCIAL IS STAMPED WITH                  
         BE    VR00              VITA SESSION WEB APPLICATION ID                
         CLC   =C'TS',SVWID                                                     
         BE    VR00                                                             
         CLC   =C'RS',SVWID                                                     
         BNE   VR10                                                             
VR00     CLI   ACTUAL,2          CANNOT ADD VERSION 2                           
         JE    WEBERR                                                           
                                                                                
VR10     MVC   SVVER,ACTUAL      SAVE VERSION CODE                              
                                                                                
         OC    VERAVID,SPACES                                                   
                                                                                
         CLI   COVRTBL,X'FF'     IF THIS IS THE FIRST VERSION                   
         BNE   VR20              BEING ADDED FOR THE COMMERCIAL                 
         CLI   ACTUAL,1          VERSION CODE MUST BE 1                         
         BNE   FLDINV                                                           
         CLC   VERAVID,SVCOCID   VERSION ID MUST MATCH MAIN COMM'L              
         BNE   AIDINV                                                           
         LA    R2,VERAVLNH                                                      
         GOTO1 VALINUM           AND VERSION LENGTH MUST MATCH MAIN             
         CLC   ACTUAL,SVCOSEC    COMM'L                                         
         BNE   FLDINV                                                           
         MVC   SVVRLEN,ACTUAL                                                   
         B     VR50                                                             
                                                                                
         USING COVRTBLD,RE                                                      
VR20     LA    RE,COVRTBL        IF THIS IS NOT THE FIRST VERSION               
VR30     CLI   0(RE),X'FF'       BEING ADDED FOR THE COMMERCIAL                 
         BE    VR40                                                             
         CLC   SVVER,COVRCD      VERSION CODE CANNOT ALREADY EXIST              
         BE    FLDINV            ON COMMERCIAL                                  
         LA    RE,COVRLNQ(RE)                                                   
         B     VR30                                                             
         DROP  RE                                                               
                                                                                
VR40     LA    R2,VERAVIDH       VERS ID CANNOT ALREADY EXIST ON SYSTEM         
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'04',VERAVIDH)                    
         BE    FLDINV                                                           
         LA    R2,VERAVLNH       VALIDATE LENGTH                                
         GOTO1 VALINUM                                                          
         MVC   SVVRLEN,ACTUAL                                                   
         LA    R2,VERAV26H       VALIDATE LENGTH                                
         MVI   SV26KF,C'N'                                                      
         CLI   SVVER,4           CAN ONLY BE 26K FOR VERSION 4 OR               
         BNL   *+20              HIGHER                                         
         CLI   8(R2),C'Y'                                                       
         BE    FLDINV                                                           
         CLI   5(R2),0                                                          
         BE    VR50                                                             
*                                                                               
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         CLI   8(R2),C'Y'                                                       
         BE    VR42                                                             
         CLI   8(R2),C'N'                                                       
         BNE   FLDINV                                                           
         B     VR50                                                             
VR42     MVI   SV26KF,C'Y'                                                      
*                                                                               
*                                                                               
                                                                                
VR50     LA    R2,VERAVALH       IF ALIAS CODE IS ENTERED                       
         CLI   5(R2),0                                                          
         BE    VR100                                                            
         GOTO1 VALINUM           ALIAS CODE MUST BE VALID NUMERIC               
         CLI   ACTUAL,1          BETWEEN 1 AND 250                              
         BL    FLDINV                                                           
         CLI   ACTUAL,250                                                       
         BH    FLDINV                                                           
         CLC   SVVER,ACTUAL      AND CANNOT MATCH VERSION CODE                  
         BE    ERRVANOM                                                         
                                                                                
         USING COVRTBLD,RE                                                      
         LA    RE,COVRTBL        ALIAS CODE MUST MATCH AN ALREADY               
VR60     CLI   0(RE),X'FF'       EXISTING VERSION CODE                          
         BE    ERRVAVRN                                                         
         CLC   ACTUAL,COVRCD                                                    
         BE    VR70                                                             
         LA    RE,COVRLNQ(RE)                                                   
         B     VR60                                                             
                                                                                
VR70     CLI   COVRIAL,0         VERSION BEING ALIASED CANNOT HAVE              
         BNE   ERRVAHAS          AN ALIAS                                       
         MVI   COVRALBY,C'Y'                                                    
                                                                                
         LA    RE,COVRTBL        ADD THIS VERSION TO THE COMMERCIAL             
VR80     CLI   0(RE),X'FF'       VERSION TABLE                                  
         BE    VR90                                                             
         LA    RE,COVRLNQ(RE)                                                   
         B     VR80                                                             
VR90     MVC   COVRCD,SVVER                                                     
         MVC   COVRID,VERAVID                                                   
         MVC   COVRLN,SVVRLEN                                                   
         MVI   COVRIAL,0                                                        
         MVC   COVRNAL,ACTUAL                                                   
         MVI   COVRLNQ(RE),X'FF'                                                
         DROP  RE                                                               
                                                                                
         USING TAVRD,R4                                                         
VR100    LA    R4,ELEMENT        BUILD VERSION ELEMENT                          
         MVI   TAVREL,TAVRELQ                                                   
         MVI   TAVRLEN,TAVRLNQ                                                  
         MVC   TAVRVERS,SVVER    VERSION CODE                                   
         MVC   TAVRCID,VERAVID   VERSION ID                                     
         MVC   TAVRSEC,SVVRLEN   VERSION LENGTH                                 
         B     VR150                                                            
         DROP  R4                                                               
                                                                                
VR110    LA    R2,VERAVTIH       IF DELETING A VERSION ...                      
         CLI   5(R2),0           TITLE CANNOT BE PROVIDED                       
         BNE   FLDINV                                                           
                                                                                
         LA    R2,VERDVCDH                                                      
         GOTO1 VALINUM           VERSION CODE MUST BE VALID NUMERIC             
                                                                                
         CLI   ACTUAL,1          CANNOT DELETE VERSION 1                        
         BE    FLDINV                                                           
                                                                                
         CLC   =C'VS',SVWID      IF COMMERCIAL IS STAMPED WITH                  
         BE    VR115             VITA SESSION WEB APPLICATION ID                
         CLC   =C'TS',SVWID                                                     
         BNE   VR120                                                            
VR115    CLI   ACTUAL,2          CANNOT DELETE VERSION 2                        
         JE    WEBERR                                                           
                                                                                
VR120    MVC   SVVER,ACTUAL                                                     
                                                                                
         USING COVRTBLD,RE                                                      
         LA    RE,COVRTBL        AND VERSION CODE MUST ALREADY                  
VR130    CLI   0(RE),X'FF'       EXIST ON COMMERCIAL                            
         BE    ERRVAVRN                                                         
         CLC   SVVER,COVRCD                                                     
         BE    VR140                                                            
         LA    RE,COVRLNQ(RE)                                                   
         B     VR130                                                            
                                                                                
VR140    CLI   COVRVC,C'Y'       CANNOT DELETE IF VERSION HAS                   
         BE    WEBERR            VITA COMPLETIONS STAMP                         
                                                                                
         CLI   COVRALBY,0        CANNOT DELETE IF BEING ALIASED                 
         BNE   ERRVANOD                                                         
                                                                                
         MVI   COVRNAL,0         SET TO DELETE ALIAS RECORD TOO                 
         DROP  RE                                                               
                                                                                
         USING TLVRD,RF                                                         
         LA    RF,KEY            READ VERSION KEY.RECORD                        
         XC    KEY,KEY                                                          
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,TGCOM                                                    
         MVC   TLVRVER,SVVER                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLVRKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         DROP  RF                                                               
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ    ENSURE IT DOES NOT HAVE ANY                    
         BAS   RE,GETEL          TRACKS ATTACHED                                
         BE    ERRVTNOD                                                         
                                                                                
VR150    GOTO1 FLDVAL,DMCB,(X'40',VERVR1H),(X'80',VERVRLH)                      
         BE    VR250                                                            
                                                                                
         LA    R2,VERVR1H                                                       
         LA    R3,VERVRLH                                                       
                                                                                
VR160    CLI   5(R2),0                                                          
         BE    VR240                                                            
         CR    R2,R3                                                            
         BH    VR240                                                            
                                                                                
         GOTO1 VALINUM                                                          
         MVC   CURVER,ACTUAL     SAVE CURRENT VERSION CODE                      
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE             BUMP TO VERSION ID FIELD                       
         ZIC   RE,0(R2)                                                         
         AR    R2,RE             BUMP TO VERSION LENGTH FIELD                   
                                                                                
         ZIC   RE,0(R2)          BUMP TO VERSION 26K FIELD                      
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE             BUMP TO VERSION ALIAS FIELD                    
                                                                                
         MVI   ACTUAL,0                                                         
         CLI   5(R2),0                                                          
         BE    VR210                                                            
         GOTO1 VALINUM           ALIAS CODE MUST BE VALID NUMERIC               
         CLI   ACTUAL,1          BETWEEN 1 AND 250                              
         BL    FLDINV                                                           
         CLI   ACTUAL,250                                                       
         BH    FLDINV                                                           
         CLC   CURVER,ACTUAL     AND CANNOT MATCH VERSION CODE                  
         BE    ERRVANOM                                                         
                                                                                
         USING COVRTBLD,RE                                                      
         LA    RE,COVRTBL                                                       
VR170    CLI   0(RE),X'FF'       THIS VERSION CANNOT BE THE ALIAS               
         BNE   *+6               FOR ANOTHER VERSION                            
         DC    H'00'                                                            
         CLC   CURVER,COVRCD                                                    
         BE    VR180                                                            
         LA    RE,COVRLNQ(RE)                                                   
         B     VR170                                                            
VR180    CLI   COVRALBY,0                                                       
         BNE   ERRVAEXA                                                         
                                                                                
         TM    4(R2),X'20'       ERROR IF VERSION IS STAMPED WITH               
         BO    VR185             VITA COMPLETION WEB APPLICATION ID             
         CLI   COVRVC,C'Y'                                                      
         JE    WEBERR                                                           
                                                                                
VR185    LA    RE,COVRTBL        ALIAS CODE MUST MATCH AN ALREADY               
VR190    CLI   0(RE),X'FF'       EXISTING VERSION CODE                          
         BE    ERRVAVRN                                                         
         CLC   ACTUAL,COVRCD                                                    
         BE    VR200                                                            
         LA    RE,COVRLNQ(RE)                                                   
         B     VR190                                                            
                                                                                
VR200    CLI   COVRNAL,0         VERSION BEING ALIASED CANNOT HAVE              
         BNE   ERRVAHAS          AN ALIAS                                       
         MVI   COVRALBY,C'Y'                                                    
                                                                                
VR210    LA    RE,COVRTBL                                                       
VR220    CLI   0(RE),X'FF'       FIND CURRENT VERSION IN VERSION                
         BNE   *+6               TABLE                                          
         DC    H'00'                                                            
         CLC   COVRCD,CURVER                                                    
         BE    VR230                                                            
         LA    RE,COVRLNQ(RE)                                                   
         B     VR220                                                            
VR230    MVC   COVRNAL,ACTUAL    SAVE NEW ALIAS CODE                            
         DROP  RE                                                               
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     VR160                                                            
                                                                                
VR240    GOTO1 FLDVAL,DMCB,(X'80',VERAVCDH),(X'80',VERDVCDH)                    
         BE    VR330                                                            
                                                                                
         USING VINDEXD,RE                                                       
VR250    LA    RE,VERINDEX       FIND RECORD EQUATE FOR THIS                    
VR260    CLI   0(RE),X'FF'       VERSION NUMBER                                 
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   SVVER,VINDUPLM                                                   
         BNH   VR270                                                            
         LA    RE,VINDLNQ(RE)                                                   
         B     VR260                                                            
                                                                                
VR270    L     RF,ASVPBLK        CLEAR INITIAL POINTER BLOCK                    
         XC    0(255,RF),0(RF)                                                  
                                                                                
         USING TLCOD,RF                                                         
         LA    RF,KEY            SEE IF RECORD ALREADY EXISTS                   
         MVC   KEY,SVCOKEY                                                      
         MVC   TLCOVER,VINDEQUT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BE    VR280                                                            
         DROP  RE,RF                                                            
                                                                                
         L     RE,AIO                                                           
         XC    0(255,RE),0(RE)                                                  
         MVC   0(L'TLCOKEY,RE),KEYSAVE                                          
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         B     VR320                                                            
                                                                                
VR280    MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,ASVPBLK                                             
         OC    ELEMENT,ELEMENT                                                  
         BZ    VR290                                                            
         GOTO1 ADDELEM                                                          
                                                                                
         CLI   SVVER,1             IF ADDING VERSION 1                          
         BNE   VR310                                                            
         CLI   VERAVTIH+5,0        AND TITLE PROVIDED                           
         BE    VR310               OVERWRITE COMMERCIAL TITLE                   
         GOTO1 NAMIN,DMCB,TANAELQ,VERAVTIH                                      
         B     VR310                                                            
                                                                                
         USING TAVRD,R4                                                         
VR290    L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR400    BAS   RE,NEXTEL                                                        
         BNE   VR310                                                            
         CLC   TAVRVERS,SVVER                                                   
         BNE   VR400                                                            
         MVI   TAVREL,X'FF'                                                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R4                                                               
                                                                                
VR310    BAS   RE,UNVERIFY                                                      
         GOTO1 PUTREC                                                           
                                                                                
VR320    GOTO1 ADDPTRS,DMCB,(X'08',ASVPBLK),AADPBLK                             
                                                                                
VR330    BAS   RE,PROALIAS                                                      
                                                                                
         BAS   RE,PROVREC                                                       
                                                                                
         MVI   IOOPT,C'Y'                                                       
         BAS   RE,RESTORE                                                       
                                                                                
         GOTO1 FLDVAL,DMCB,(X'80',VERAVCDH),(X'80',VERDVCDH)                    
         BE    XIT                                                              
                                                                                
         CLC   SVVER,VERINDEX                                                   
         BNH   XIT                                                              
         BAS   RE,UNVERIFY                                                      
         GOTO1 PUTREC                                                           
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO BUILD VERSION TABLE FOR COMMERCIAL                *         
***********************************************************************         
                                                                                
BLDCOTBL NTR1                                                                   
         LA    RE,COVRTBL        CLEAR COMMERCIAL VERSION TABLE                 
         LHI   RF,COVRLNQ                                                       
BCT10    XC    0(250,RE),0(RE)                                                  
         LA    RE,250(RE)                                                       
         BCT   RF,BCT10                                                         
                                                                                
         MVC   AIO,AIO2                                                         
         LA    R5,KEY                                                           
                                                                                
         USING COVRTBLD,R2                                                      
         LA    R2,COVRTBL        R2=A(COMMERCIAL VERSION TABLE)                 
                                                                                
         MVC   KEY,SVCOKEY       READ COMMERCIAL RECORD                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ    IF COMMERCIAL HAS VERSION 1 ELEMENT            
         BRAS  RE,GETEL                                                         
         BNE   BCT80                                                            
         MVC   COVRCD,TAVRVERS   SAVE VERSION CODE                              
         MVC   COVRID,TAVRCID    ID                                             
         MVC   COVRLN,TAVRSEC    AND LENGTH IN COMMERCIAL VERSION TABLE         
         DROP  R4                                                               
                                                                                
         USING TLVRD,RE                                                         
         LA    RE,SVVRKEY        INITALIZE SAVED VERSION KEY                    
         XC    SVVRKEY,SVVRKEY                                                  
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,SVCOKEY+TLCOCOM-TLCOD                                    
         DROP  RE                                                               
                                                                                
         USING TAFND,R4                                                         
BCT20    MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   BCT25                                                            
         L     R4,TGELEM                                                        
         CLC   =C'VC',TAFNNAME                                                  
         BE    BCT24                                                            
         CLC   =C'TC',TAFNNAME                                                  
         BE    BCT24                                                            
         CLC   =C'RC',TAFNNAME                                                  
         BNE   BCT25                                                            
BCT24    MVI   COVRVC,C'Y'                                                      
         DROP  R4                                                               
                                                                                
         USING TLAKD,R5                                                         
BCT25    XC    KEY,KEY           READ FOR ALIAS RECORD                          
         MVI   TLAKCD,TLAKCDQ                                                   
         MVC   TLAKAGY,TGAGY                                                    
         MVC   TLAKADID,COVRID                                                  
         GOTO1 HIGH                                                             
         B     BCT40                                                            
BCT30    GOTO1 SEQ                                                              
BCT40    CLC   KEY(TLAKNCLI-TLAKD),KEYSAVE                                      
         BNE   BCT70                                                            
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPX))                                     
         BNE   BCT30                                                            
                                                                                
         USING TACMD,RE                                                         
         L     RE,TGELEM                                                        
         B     BCT60                                                            
                                                                                
BCT50    ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),TACMELQ                                                    
         BNE   BCT30                                                            
         CLI   TACMTYPE,TACMTYPX                                                
         BNE   BCT30                                                            
                                                                                
BCT60    CLC   COVRID,TACMCOMM                                                  
         BNE   BCT50                                                            
         DROP  RE                                                               
                                                                                
         MVC   COVRIAL,TLAKVER   SAVE INITIAL ALIAS CODE                        
         MVC   COVRNAL,TLAKVER                                                  
         DROP  R5                                                               
                                                                                
BCT70    LA    R2,COVRLNQ(R2)                                                   
                                                                                
         USING TLVRD,R5                                                         
         MVC   KEY,SVVRKEY                                                      
         ZIC   RE,TLVRVER                                                       
         AHI   RE,1                                                             
         STC   RE,TLVRVER                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(TLVRVER-TLVRCD),KEYSAVE                                      
         BNE   BCT80                                                            
         MVC   SVVRKEY,KEY                                                      
         GOTO1 GETREC                                                           
         DROP  R5                                                               
                                                                                
         USING TLVRD,R4                                                         
         L     R4,AIO                                                           
         MVC   COVRCD,TLVRVER    SAVE VERSION CODE                              
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   COVRID,TACOCID    ID                                             
         MVC   COVRLN,TACOSEC    AND LENGTH IN COMMERCIAL VERSION TABLE         
         TM    TACOSTA3,TACOS26K                                                
         BNO   *+8                                                              
         MVI   COV26K,C'Y'       VERSION IS 26K                                 
         B     BCT20                                                            
         DROP  R4                                                               
                                                                                
BCT80    MVI   0(R2),X'FF'       MARK END OF VERSION TABLE                      
                                                                                
         LA    R2,COVRTBL        R2=A(COMMERCIAL VERSION TABLE)                 
BCT90    CLI   0(R2),X'FF'                                                      
         BE    BCT130                                                           
         CLI   COVRIAL,0         IF VERSION IS AN ALIAS TO ANOTHER              
         BE    BCT120                                                           
                                                                                
         LA    RE,COVRTBL                                                       
BCT100   CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   COVRIAL,0(RE)     FIND THE ALIASED VERSION                       
         BE    BCT110                                                           
         LA    RE,COVRLNQ(RE)    AND MARK THAT IS IS ALIASED                    
         B     BCT100                                                           
BCT110   MVI   COVRALBY-COVRTBLD(RE),C'Y'                                       
                                                                                
BCT120   LA    R2,COVRLNQ(R2)    BUMP TO NEXT VERSION ENTRY                     
         B     BCT90                                                            
         DROP  R2                                                               
                                                                                
BCT130   MVC   AIO,AIO1                                                         
         BAS   RE,RESTORE        AND RESTORE MAIN COMMERCIAL                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        RESTORE MAIN COMMERCIAL RECORD INTO AIO1                     *         
***********************************************************************         
                                                                                
RESTORE  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVCOKEY),SVCOKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),SVCOKEY                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 GETREC                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO MARK COMMERCIAL AS UNVERIFIED                     *         
***********************************************************************         
                                                                                
UNVERIFY NTR1                                                                   
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ    UNMARK COMMERCIAL'S VERIFIED STATUS            
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         XC    TACOVDTE,TACOVDTE                                                
         XC    TACOVTIM,TACOVTIM                                                
         XC    TACOVSTU,TACOVSTU                                                
         XC    TACOVST,TACOVST                                                  
         OI    TACOUVST,TACOUVCO                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PROCESS ALIAS RECORDS                             *         
***********************************************************************         
                                                                                
PROALIAS NTR1                                                                   
         USING COVRTBLD,R5                                                      
         LA    R5,COVRTBL                                                       
PALS10   CLI   0(R5),X'FF'                                                      
         BE    XIT                                                              
                                                                                
         CLI   COVRIAL,0         IF ALIAS RECORD DID NOT EXIST BEFORE           
         BNE   PALS20                                                           
         CLI   COVRNAL,0         BUT IT DOES NOW                                
         BE    PALS20                                                           
         BAS   RE,ADDALIAS       ADD NEW ALIAS RECORD                           
         B     PALS40                                                           
                                                                                
PALS20   CLI   COVRIAL,0         IF ALIAS RECORD DID EXIST BEFORE               
         BE    PALS40                                                           
         CLI   COVRNAL,0         BUT IT DOES NOT NOW                            
         BNE   PALS30                                                           
         BAS   RE,DELALIAS       DELETE ALIAS RECORD                            
         B     PALS40                                                           
                                                                                
PALS30   CLC   COVRIAL,COVRNAL   IF ALIAS RECORD HAS CHANGED                    
         BE    PALS40                                                           
         BAS   RE,DELALIAS       DELETE OLD ALIAS RECORD                        
         BAS   RE,ADDALIAS       AND ADD THE NEW ONE                            
                                                                                
PALS40   LA    R5,COVRLNQ(R5)                                                   
         B     PALS10                                                           
                                                                                
***********************************************************************         
*        ROUTINE TO ADD AN ALIAS RECORD                               *         
*        ON ENTRY ... R5=A(CURRENT VERSION TABLE ENTRY)               *         
***********************************************************************         
                                                                                
ADDALIAS NTR1                                                                   
         USING TLAKD,R4                                                         
         L     R4,AIO                                                           
         XC    0(255,R4),0(R4)   BUILD KEY FOR ALIAS RECORD                     
         MVI   TLAKCD,TLAKCDQ                                                   
         MVC   TLAKAGY,SVCOKEY+TLCOAGY-TLCOD                                    
         MVC   TLAKADID,COVRID                                                  
         MVC   TLAKNCLI,SVCOKEY+TLCOCLI-TLCOD                                   
         MVC   TLAKNPRD,SVCOKEY+TLCOPRD-TLCOD                                   
         MVC   TLAKMED,SVCOMED                                                  
         MVC   TLAKVER,COVRNAL                                                  
         MVC   TLAKCOM,SVCOKEY+TLCOCOM-TLCOD                                    
         DROP  R4                                                               
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLAKKEY),0(R4)                                             
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLAKKEY),KEYSAVE                                           
         BNE   AALS10                                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         NI    KEY+TLDRSTAT-TLDRD,X'FF'-X'80'                                   
         GOTO1 WRITE                                                            
         NI    TLRCSTAT-TLRCD(R4),X'FF'-X'80'                                   
         BAS   RE,ADDALELS                                                      
         GOTO1 PUTREC                                                           
         B     AALS20                                                           
                                                                                
AALS10   BAS   RE,ADDALELS                                                      
         GOTO1 ADDREC            ADD ALIAS RECORD                               
AALS20   NI    DMINBTS,X'FF'-X'08'                                              
         B     XIT                                                              
                                                                                
***********************************************************************         
*        ROUTINE TO ADD ELEMENTS TO ALIAS RECORD                      *         
*        ON ENTRY ... R5=A(CURRENT VERSION TABLE ENTRY)               *         
***********************************************************************         
                                                                                
ADDALELS NTR1                                                                   
         USING TACMD,R4                                                         
         LA    R4,ELEMENT        BUILD NEW FULL COMMERCIAL ID                   
         XC    ELEMENT,ELEMENT   ELEMENT                                        
         MVI   TACMEL,TACMELQ                                                   
         MVI   TACMLEN,TACMLNQ+L'COVRID                                         
         MVI   TACMTYPE,TACMTYPX                                                
         MVC   TACMCOMM(L'COVRID),COVRID                                        
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 ACTVIN,DMCB,0     ADD ACTIVITY ELEMENT                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DELETE AN ALIAS RECORD                            *         
*        ON ENTRY ... R5=A(CURRENT VERSION TABLE ENTRY)               *         
***********************************************************************         
                                                                                
DELALIAS NTR1                                                                   
         USING TLAKD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY           READ FOR THIS VERSION'S ALIAS RECORD           
         MVI   TLAKCD,TLAKCDQ                                                   
         MVC   TLAKAGY,SVCOKEY+TLCOAGY-TLCOD                                    
         MVC   TLAKADID,COVRID                                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     DA20                                                             
DA10     GOTO1 SEQ                                                              
DA20     CLC   KEY(TLAKNCLI-TLAKD),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPX))                                     
         BNE   DA10                                                             
                                                                                
         USING TACMD,RE                                                         
         L     RE,TGELEM                                                        
         B     DA40                                                             
                                                                                
DA30     ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),TACMELQ                                                    
         BNE   DA10                                                             
         CLI   TACMTYPE,TACMTYPX                                                
         BNE   DA10                                                             
                                                                                
DA40     CLC   COVRID,TACMCOMM                                                  
         BNE   DA30                                                             
                                                                                
         MVI   TACMEL,X'FF'                                                     
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  RE                                                               
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPX))                                     
         BE    *+8                                                              
         OI    TLAKSTAT,X'80'                                                   
         GOTO1 ACTVIN,DMCB,0     ADD ACTIVITY ELEMENT                           
         GOTO1 PUTREC            AND PUT BACK RECORD                            
         DROP  R4                                                               
                                                                                
         USING TLDRD,R4                                                         
         LA    R4,KEY                                                           
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPX))                                     
         BE    *+8                                                              
         OI    TLDRSTAT,X'80'    DELETE ALIAS KEY                               
         GOTO1 WRITE                                                            
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO PROCESS VERSION RECORD                            *         
***********************************************************************         
                                                                                
PROVREC  NTR1                                                                   
         CLI   SVVER,2             ONLY EXECUTE ROUTINE IF PROCESSING           
         BL    XIT                 VERSION 2 OR HIGHER                          
                                                                                
         CLI   VERAVCDH+5,0        IF ADDING VERSION 2 OR HIGHER                
         BE    PVR50                                                            
         BAS   RE,RESTORE          INITIALIZE AS PRIMARY COMMERCIAL             
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGCLI,TLCOCLI       SAVE CLIENT CODE                             
         MVC   TGPRD,TLCOPRD       AND PRODUCT CODE                             
         DROP  R4                                                               
                                                                                
         L     RE,ASVPBLK          INITIALIZE PASSIVE POINTER BLOCK             
         XC    0(255,RE),0(RE)                                                  
                                                                                
         USING TLVRD,R4                                                         
         XC    0(L'TLVRKEY,R4),0(R4)                                            
         MVI   TLVRCD,TLVRCDQ                                                   
         MVC   TLVRCOM,TGCOM       MOVE VERSION KEY OVER COMMERCIAL             
         MVC   TLVRVER,SVVER       KEY                                          
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,TAOCELQ      DELETE OLD AGENCY/CID ELEMENTS               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAVRELQ      DELETE VERSION ELEMENTS                      
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TAFNELQ      DELETE PRODUCT NAME ELEMENT                  
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTPRD))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTWEB))                                     
         GOTO1 DELL,DMCB,(1,=AL1(TAFNTOWB))                                     
                                                                                
         TM    TGSYSTAT,TASYSMUS   IF NEW MUSIC RULES ENABLED                   
         JZ    PVR10                                                            
         MVI   ELCODE,TAMCELQ      DELETE MUSIC CONTRACT ELEMENTS               
         GOTO1 REMELEM                                                          
         MVI   ELCODE,TATRELQ      DELETE MUSIC TRACK ELEMENTS                  
         GOTO1 REMELEM                                                          
                                                                                
         USING TACOD,R4                                                         
PVR10    MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TACOCID,VERAVID     MOVE VERSION ID/LENGTH OVER                  
         MVC   TACOSEC,SVVRLEN     COMMERCIAL ID/LENGTH                         
         CLI   SV26KF,C'Y'                                                      
         BNE   *+8                                                              
         OI    TACOSTA3,TACOS26K                                                
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     ADD AGENCY CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGAGY                                          
         MVI   TAFNTYPE,TAFNTAGY                                                
         MVC   TAFNNAME(L'TGAGY),TGAGY                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         XC    ELEMENT,ELEMENT     ADD CLIENT CODE ELEMENT                      
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGCLI                                          
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'TGCLI),TGCLI                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         USING TAFND,R4                                                         
         OC    TGPRD,TGPRD                                                      
         JZ    PVR20                                                            
         XC    ELEMENT,ELEMENT     ADD PRODUCT CODE ELEMENT                     
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNLEN,TAFNLNQ+L'TGPRD                                          
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'TGPRD),TGPRD                                          
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
PVR20    CLI   VERAVTIH+5,0        IF TITLE PROVIDED                            
         JE    PVR30               OVERWRITE COMMERCIAL TITLE                   
         GOTO1 NAMIN,DMCB,TANAELQ,VERAVTIH                                      
                                                                                
PVR30    L     R4,AIO              R4=A(VERSION RECORD)                         
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLVRKEY),0(R4)                                             
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                IF VERSION RECORD ALREADY EXISTS             
         NI    DMINBTS,X'F7'       IN DELETED STATUS                            
         CLC   KEY(L'TLVRKEY),KEYSAVE                                           
         BNE   PVR40                                                            
         NI    KEY+TLDRSTAT-TLDRD,X'7F'                                         
         GOTO1 WRITE                                                            
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              WRITE IT BACK OVER DELETED ONE               
         GOTO1 ADDPTRS,DMCB,ASVPBLK                                             
         B     XIT                                                              
                                                                                
PVR40    GOTO1 ADDREC              ELSE JUST ADD VERSION RECORD                 
         GOTO1 ADDPTRS,DMCB,ASVPBLK                                             
         B     XIT                                                              
                                                                                
***********************************************************************         
                                                                                
PVR50    CLI   VERDVCDH+5,0        IF DELETING VERSION 2 OR HIGHER              
         BE    XIT                                                              
                                                                                
         USING TLVRD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLVRCD,TLVRCDQ      READ FOR VERSION KEY/RECORD                  
         MVC   TLVRCOM,TGCOM                                                    
         MVC   TLVRVER,SVVER                                                    
         GOTO1 HIGH                                                             
         CLC   TLVRKEY,KEYSAVE                                                  
         BNE   XIT                                                              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R3                                                               
                                                                                
         GOTO1 SAVPTRS,DMCB,ASVPBLK                                             
                                                                                
         USING TLDRD,R3                                                         
         OI    TLDRSTAT,X'80'      IF FOUND, DELETE KEY                         
         GOTO1 WRITE                                                            
         DROP  R3                                                               
                                                                                
         USING TLVRD,R4                                                         
         L     R4,AIO                                                           
         OI    TLVRSTAT,X'80'      AND DELETE RECORD                            
         GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'40',ASVPBLK)                                     
         B     XIT                                                              
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ERROR MESSAGES AND EXITS                                     *         
***********************************************************************         
                                                                                
ACDINV   LA    R2,VERAVCDH         INVALID ADD CODE FIELD                       
         B     FLDINV                                                           
                                                                                
AIDINV   LA    R2,VERAVIDH         INVALID ADD ID FIELD                         
         B     FLDINV                                                           
                                                                                
ALNINV   LA    R2,VERAVLNH         INVALID ADD LENGTH ID FIELD                  
         B     FLDINV                                                           
                                                                                
DCDINV   LA    R2,VERDVCDH         INVALID DELETE CODE FIELD                    
         B     FLDINV                                                           
                                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     MSGEXIT                                                          
                                                                                
ACDMIS   LA    R2,VERAVCDH         MISSING ADD CODE FIELD                       
         B     FLDMIS                                                           
                                                                                
AIDMIS   LA    R2,VERAVIDH         MISSING ADD ID FIELD                         
         B     FLDMIS                                                           
                                                                                
ALNMIS   LA    R2,VERAVLNH         MISSING ADD LENGTH ID FIELD                  
         B     FLDMIS                                                           
                                                                                
FLDMIS   MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     MSGEXIT                                                          
                                                                                
INVTYPE  MVI   ERROR,ERRECCTY      INVALID RECORD FOR COMMERICIAL TYPE          
         B     MSGEXIT                                                          
                                                                                
WEBERR   MVC   MYMSGNO,=Y(ERUSEWEB) RECORD MUST BE UPDATED FROM WEB             
         B     EXTEXIT              APPLICATION                                 
                                                                                
ERRVANOM MVC   MYMSGNO,=Y(ERVANOMT) ALIAS CANNOT MATCH THIS VERSION             
         B     EXTEXIT                                                          
                                                                                
ERRVAVRN MVC   MYMSGNO,=Y(ERVAVRNE) VERSION DOES NOT EXIST                      
         B     EXTEXIT                                                          
                                                                                
ERRVAHAS MVC   MYMSGNO,=Y(ERVAHASA) VERSION ALREADY HAS AN ALIAS                
         B     EXTEXIT                                                          
                                                                                
ERRVAEXA MVC   MYMSGNO,=Y(ERVAEXAL) EXISTING ALIASES POINT AT VERSION           
         B     EXTEXIT                                                          
                                                                                
ERRVANOD MVC   MYMSGNO,=Y(ERVANODL) CANNOT DELETE IF IT HAS ALIASES             
         B     EXTEXIT                                                          
                                                                                
ERRVTNOD MVC   MYMSGNO,=Y(ERVTNODL) CANNOT DELETE VER IT IF HAS TRACKS          
         B     EXTEXIT                                                          
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     EXTEXIT                                                          
                                                                                
EXTEXIT  MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     MSGEXIT                                                          
                                                                                
RECEXIT  LA    R2,CONRECH                                                       
MSGEXIT  GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
***********************************************************************         
*        TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE VERSION IS ON *         
***********************************************************************         
                                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        PF KEY TABLE                                                 *         
***********************************************************************         
                                                                                
PFTABLE  DC    AL1(PF19X-*,19,0,(PF19X-PF19)/KEYLNQ,0)                          
         DC    CL3' ',CL8'VCAST   ',CL8'LIST    '                               
PF19     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYTWA,L'VERCID-1),AL2(VERCID-T702FFD)                     
PF19X    EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
HEXFFS   DC    16X'FF'                                                          
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECT TO COVER TABLE THAT DETERMINES WHICH COMMERCIAL        *         
*        RECORD THE VERSION CODE IS ON                                *         
***********************************************************************         
                                                                                
VINDEXD  DSECT                                                                  
VINDUPLM DS    X                 RECORD'S UPPER LIMIT                           
VINDEQUT DS    X                 RECORD'S EQUATE                                
VINDLNQ  EQU   *-VINDEXD                                                        
         EJECT                                                                  
***********************************************************************         
*        DSECT TO COVER COMMERCIAL VERSION TABLE ENTRY                *         
***********************************************************************         
                                                                                
COVRTBLD DSECT                                                                  
COVRCD   DS    X                 VERSION CODE                                   
COVRID   DS    CL12              VERSION ID                                     
COVRLN   DS    X                 VERSION LENGTH                                 
COVRIAL  DS    X                 VERSION INITIAL ALIAS                          
COVRNAL  DS    X                 VERSION NEW ALIAS                              
COVRALBY DS    C                 VERSION IS ALIASED BY ANOTHER VERSION          
COVRVC   DS    C                 VERSION HAS VITA COMPLETIONS LOCK              
COV26K   DS    C                 VERSION IS 26K                                 
COVRLNQ  EQU  *-COVRTBLD                                                        
         EJECT                                                                  
***********************************************************************         
*        DSECT TO COVER TEMPORARY STORAGE                             *         
***********************************************************************         
                                                                                
TMPD     DSECT                                                                  
COVRTBL  DS    XL((250*COVRLNQ)+1)                                              
SVPTRBLK DS    CL((520*L'TLDRREC)+1) INITIAL POINTERS                           
ADPTRBLK DS    CL((520*L'TLDRREC)+1) UPDATED POINTERS                           
TMPLNQ   EQU   *-TMPD                                                           
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRDBD                                                       
         EJECT                                                                  
***********************************************************************         
*        LOCAL VARIABLES                                              *         
***********************************************************************         
                                                                                
         DS    100X                                                             
                                                                                
ASVPBLK  DS    A                 INITIAL POINTER BLOCK                          
AADPBLK  DS    A                 UPDATED POINTER BLOCK                          
                                                                                
SVCOKEY  DS    XL(L'KEY)         SAVED COMMERCIAL KEY                           
SVVRKEY  DS    XL(L'KEY)         SAVED VERSION KEY                              
SVCOCID  DS    XL(L'TGCID)       SAVED COMMERCIAL ID                            
SVCOMED  DS    XL(L'TACOMED)     SAVED COMMERCIAL MEDIA                         
SVCOSEC  DS    XL(L'TACOSEC)     SAVED COMMERCIAL LENGTH                        
                                                                                
SVWID    DS    CL18              SAVED WEB APPLICATION ID                       
                                                                                
SVVER    DS    XL(L'TGVER)       SAVED VERSION CODE                             
SVVRLEN  DS    XL(L'TAVRSEC)     SAVED VERSION LENGTH                           
SV26KF   DS    C                                                                
                                                                                
CURVER   DS    XL(L'TGVER)       CURRENT VERSION CODE                           
CURCID   DS    XL(L'TGCID)       CURRENT VERSION ID                             
                                                                                
LSTDVER  DS    X                 LAST DISPLAYED VERSION                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012TAGENDB   05/29/15'                                      
         END                                                                    
