*          DATA SET BUFIL01    AT LEVEL 002 AS OF 08/11/00                      
*PHASE T50201A                                                                  
         TITLE 'T50201 - BUDGET CONTROL LFM - AGENCY RECORD'                    
T50201   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FI01**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         GOTO1 VSETADD                                                          
*                                                                               
         MVI   ERROR,INVREC                                                     
         LA    R2,CONRECH                                                       
         CLI   DDS,YES             DDS TERMINALS ONLY                           
         BNE   TRAPERR                                                          
*                                                                               
         MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTDEL       TEST FOR DELETE                              
         BE    TRAPERR             YES-DISALLOW IT                              
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         XC    KEY,KEY                                                          
         LA    R2,MASAGYH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST         EDIT FIELD FROM START                        
         XC    FTERM,FTERM                                                      
         GOTO1 VFVAL                                                            
*                                                                               
         CLI   FLDH+5,0                                                         
         BNE   VKEY1                                                            
         MVI   ERROR,MISSING                                                    
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BE    VKEYX               OK TO HAVE NO START KEY                      
         B     TRAPERR                                                          
*                                                                               
VKEY1    MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,2                                                         
         BNE   TRAPERR                                                          
         MVI   BUKSYS,C'B'                                                      
         MVC   BUKAGY,FLD                                                       
         MVI   BUKRTYP,BUKRTYPQ                                                 
         CLI   ACTNUM,ACTLIST      TEST FOR LIST                                
         BNE   *+10                                                             
         MVC   SAVEKEY,KEY         KEEP START KEY FOR SUBSEQUENT LIST           
*                                                                               
VKEYX    B     XIT                                                              
         SPACE 2                                                                
*        DISPLAY KEY                                                            
*                                                                               
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         MVC   MASAGY,BUKAGY                                                    
         OI    MASAGYH+6,X'80'     SET TRANSMIT BIT                             
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE MASTER RECORD                                                        
*                                                                               
VREC     L     R4,AIO                                                           
         USING BURECD,R4                                                        
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VR2                                                              
         L     R5,ANODBLK          MAKE RECORD A NODIO MASTER                   
         USING NODBLKD,R5                                                       
         MVC   BURLEN,DATADISP                                                  
*                                                                               
         MVI   NDWRITE,C'N'    DONT LET NODIO ACTUALLY ADD RECORD               
*                              NOTE- MOST NODIO FIELDS ARE SET BY BASE          
*                                    OR BY DEFAULT                              
         MVI   NDNDPOS,BUKNODE-BUKEY    NODE POSITION                           
*                                                                               
         MVC   NDIOA,AIO           USE NORMAL IOA                               
         GOTO1 VNODIO,DMCB,ANODBLK,=C'MAST'                                     
         MVC   NDIOA,AIO2          NODIO NORMALLY USES 2 (AND 3)                
         MVI   NDWRITE,C'Y'                                                     
*                                                                               
VR2      LA    R6,ELEM             BUILD AGENCY DESCRIPTION ELEMENT             
         USING BUAGYD,R6                                                        
*                                                                               
         MVI   ELCODE,BUAGYELQ                                                  
         GOTO1 REMELEM             GET RID OF EXISTING                          
*                                                                               
         XC    ELEM(BUAGYLNQ),ELEM                                              
         MVI   BUAGYEL,BUAGYELQ    ELEM CODE                                    
         MVI   BUAGYLEN,BUAGYLNQ   LENGTH                                       
*                                                                               
         LA    R2,MASNAMH          AGENCY NAME                                  
         GOTO1 VGETFLD,DMCB,(R2)   REQUIRED                                     
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   BUAGYNAM,FLD                                                     
*                                                                               
VR3      LA    R2,MASTAGYH                                                      
         GOTO1 VGETFLD,PARAS,(R2)                                               
         CLI   FLDH+5,0            TEST IF TALENT AGENCY INPUT                  
         BE    VR4                 NO                                           
         MVI   ERROR,INVALID                                                    
         CLI   FLDH+5,2                                                         
         BNE   TRAPERR                                                          
         MVC   BUAGYTAG,FLD                                                     
         B     VR5                                                              
*                                                                               
VR4      GOTO1 VGETFLD,PARAS,MASTUNIH                                           
         CLI   FLDH+5,0            TEST IF TALENT UNIT INPUT                    
         BNE   VR4A                YES                                          
         GOTO1 VGETFLD,PARAS,MASTLEDH                                           
         CLI   FLDH+5,0                                                         
         BE    VR6                                                              
*                                                                               
VR4A     LA    R2,MASTAGYH         FORCE TALENT AGENCY INPUT IF                 
         MVI   ERROR,MISSING       UNIT OR LEDGER INPUT                         
         B     TRAPERR                                                          
*                                                                               
VR5      LA    R2,MASTUNIH                                                      
         GOTO1 VGETFLD,PARAS,(R2)                                               
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   BUAGYTUN,FLD        EXTRACT UNIT CODE                            
*                                                                               
         LA    R2,MASTLEDH                                                      
         GOTO1 VGETFLD,PARAS,(R2)                                               
         MVI   ERROR,MISSING                                                    
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         MVC   BUAGYTLE,FLD        EXTRACT LEDGER CODE                          
*                                                                               
VR6      GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY MASTER RECORD                                                         
*                                                                               
DREC     GOTO1 VCLEARF,DMCB,MASNAMH,MASLAST                                     
         MVI   ELCODE,BUAGYELQ     GET AGENCY DESCRIPTION ELEMENT               
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,AIO),0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING BUAGYD,R6                                                        
         MVC   MASNAM,BUAGYNAM                                                  
*                                                                               
         OC    BUAGYTAL,BUAGYTAL   TEST FOR TALENT DATA                         
         BZ    DRECX                                                            
         MVC   MASTAGY,BUAGYTAG    TALENT AGENCY                                
         MVC   MASTUNI,BUAGYTUN    UNIT                                         
         MVC   MASTLED,BUAGYTLE    LEDGER                                       
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
* LIST AGENCY RECORDS                                                           
*                                                                               
LIST     LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         OC    KEY,KEY             TEST FOR GENCON SUPPLIED KEY                 
         BNZ   LIST4                                                            
         MVC   KEY,SAVEKEY         NO-TRY USER PROVIDED KEY                     
         OC    KEY,KEY             TEST FOR USER KEY                            
         BNZ   LIST1               YES                                          
         MVI   BUKSYS,C'B'         NO-SET SYSTEM CODE                           
         SPACE 1                                                                
LIST1    GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLI   BUKSYS,C'B'         TEST FOR BUDGET SYSTEM                       
         BNE   XIT                 NO                                           
         CLI   BUKRTYP,BUKRTYPQ    TEST FOR NODAL RECORD TYPE                   
         BE    LIST6               YES                                          
         BH    LIST5               PAST NODAL FILE FOR THIS AGENCY              
         MVI   BUKRTYP,BUKRTYPQ    LOW KEY                                      
         XC    BUKNODE(BUKCTL-BUKNODE),BUKNODE                                  
         B     LIST1               TRY TO FIND NODAL RECORD FOR AGENCY          
*                                                                               
LIST5    MVI   BUKRTYP,BUKRTYPQ    HIGH KEY-BUMP TO NEXT AGY/NODAL              
         SR    R1,R1                                                            
         ICM   R1,3,BUKAGY                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,BUKAGY                                                      
         XC    BUKNODE(BUKCTL-BUKNODE),BUKNODE                                  
         B     LIST1                                                            
*                                                                               
LIST6    OC    BUKNODE,BUKNODE     TEST FOR ZERO NODE                           
         BZ    LIST7                                                            
*                                                                               
         SR    R1,R1               BUMP TO NEXT AGENCY                          
         ICM   R1,3,BUKAGY                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,BUKAGY                                                      
         XC    BUKNODE(BUKCTL-BUKNODE),BUKNODE                                  
         B     LIST1                                                            
*                                                                               
LIST7    GOTO1 GETREC                                                           
*                                                                               
         MVC   MALHED(L'LISTHD),LISTHD   SET HEADLINE                           
         OI    MALHEDH+6,X'80'                                                  
         MVC   MALHED2(L'LISTHD2),LISTHD2 SET SECOND HEADLINE                   
         OI    MALHED2H+6,X'80'                                                 
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         USING LSTLIND,R5                                                       
         MVC   LSTAGY,BUKAGY                                                    
*                                                                               
LIST8    MVI   ELCODE,BUAGYELQ                                                  
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(ELCODE,AIO),0                          
         L     R6,12(R1)                                                        
         USING BUAGYD,R6                                                        
         MVC   LSTNAM,BUAGYNAM     EXTRACT AGENCY NAME                          
         OC    BUAGYTAL,BUAGYTAL   TEST FOR TALENT DATA                         
         BZ    LIST9                                                            
         MVC   LSTTAGY,BUAGYTAG                                                 
         MVC   LSTTUN,BUAGYTUN                                                  
         MVC   LSTTLE,BUAGYTLE                                                  
*                                                                               
LIST9    GOTO1 LISTMON                                                          
         B     LIST2                                                            
         DROP  R4,R5,R6                                                         
         SPACE 2                                                                
LISTHD   DC    C'AGENCY   NAME                  ---TALENT DATA----'             
LISTHD2  DC    C'                               AGENCY UNIT LEDGER'             
         EJECT                                                                  
* ROUTINES ETC.                                                                 
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER MAINTENANCE SCREEN                                             
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILF1D                                                       
         EJECT                                                                  
* DSECT TO COVER LIST SCREEN                                                    
*                                                                               
         ORG   CONTAGH                                                          
*      ++INCLUDE BUFILE1D                                                       
         EJECT                                                                  
* DSECT TO COVER OVERLAY WORKING STORAGE                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
SAVEKEY  DS    CL(L'KEY)                                                        
         SPACE 2                                                                
* DSECT TO COVER LIST DISPLAY LINE                                              
*                                                                               
LSTLIND  DSECT                                                                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LSTAGY   DS    CL6                                                              
         DS    CL3                                                              
LSTNAM   DS    CL20                                                             
         DS    CL4                                                              
LSTTAGY  DS    CL2                                                              
         DS    CL4                                                              
LSTTUN   DS    C                                                                
         DS    CL5                                                              
LSTTLE   DS    C                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002BUFIL01   08/11/00'                                      
         END                                                                    
