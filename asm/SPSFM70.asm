*          DATA SET SPSFM70    AT LEVEL 001 AS OF 10/01/07                      
*PHASE T21770A                                                                  
T21770   TITLE 'SPSFM70 - DEAL PROGRAM'                                         
***********************************************************************         
*                                                                               
*  TITLE:        T21770  -- DEAL MAINT/LIST/REPORT                              
*                T2973D  -- DEAL MAINT SCREEN                                   
*                T2973C  -- DEAL LIST SCREEN                                    
*                T2973B  -- DEAL REPORT SCREEN                                  
*                                                                               
*  COMMENTS:     MAINTAINS DEAL RECORDS                                         
*                                                                               
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                           
*                GEGENCON (T00A30), WHICH CALLS THIS.                           
*                                                                               
*  INPUTS:       SCREEN SCSFM3D (MAINT) & SCSFM3C (LIST)                        
*                                                                               
*  OUTPUTS:                                                                     
*                                                                               
*  REGISTERS:    R0 -- WORK                                                     
*                R1 -- WORK                                                     
*                R2 -- SCREEN FIELD HEADER                                      
*                R3 -- WORK                                                     
*                R4 -- WORK                                                     
*                R5 -- WORK                                                     
*                R6 -- GETEL REGISTER                                           
*                R7 -- SECOND BASE                                              
*                R8 -- SPOOL                                                    
*                R9 -- SYSD                                                     
*                RA -- TWA                                                      
*                RB -- FIRST BASE                                               
*                RC -- GEND                                                     
*                RD -- SYSTEM                                                   
*                RE -- SYSTEM                                                   
*                RF -- SYSTEM                                                   
*                                                                               
***********************************************************************         
T21770   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21770,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         USING OFFICED,OFCBLK                                                   
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       DISPLAY RECORD                               
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT?                                
         BE    PR                                                               
*                                  RELO                                         
         CLI   MODE,XRECPUT        AFTER A CHANGE?                              
         BE    XRCPUT                                                           
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
         MVI   MISCFLG1,0                                                       
*                                                                               
         LA    R2,DLMMEDH          MEDIA                                        
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,DLLMEDH                                                       
*                                                                               
         TM    4(R2),X'20'         MEDIA PREVIOUSLY VALIDATED?                  
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   THE KEY HAS CHANGED                          
*                                                                               
         MVI   SVAGMD,0                                                         
         MVI   SVAGMDLO,0                                                       
         MVI   SVAGMDHI,0                                                       
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK000               YES                                          
         CLI   ACTNUM,ACTREP       REPORT?                                      
         BE    *+12                                                             
         CLI   ACTNUM,ACTLIST         OR LIST?                                  
         BNE   ERRMIS              NO, THIS IS REQUIRED                         
         MVI   8(R2),C'T'          NEED AGENCY PORTION OF BAGYMD                
         MVI   5(R2),1                                                          
         GOTO1 VALIMED                                                          
         MVC   SVAGMD,BAGYMD                                                    
         NI    SVAGMD,X'F0'        STRIP MEDIA AS USER WANTS ALL                
*                                                                               
         MVC   SVAGMDLO,BAGYMD     SETUP HIGH AND LOW RANGE FOR AGMD            
         MVC   SVAGMDHI,BAGYMD                                                  
         OI    SVAGMDHI,X'0F'                                                   
         MVI   8(R2),0             WE                                           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
         CLI   ACTNUM,ACTREP                                                    
         BE    VK006                                                            
         B     VK003                                                            
*                                                                               
VK000    GOTO1 VALIMED                                                          
         MVC   SVAGMD,BAGYMD                                                    
*                                                                               
         CLI   ACTNUM,ACTREP       REPORT SCREEN ONLY HAS MEDIA FIELD           
         BE    VK006                                                            
         CLI   ACTNUM,ACTLIST      LIST SCREEN DOES NOT HAVE DEAL #             
         BE    VK003                   IN THE KEY SECTION                       
         LA    R2,DLMDNOH          DEAL NUMBER                                  
         TM    4(R2),X'20'         MEDIA PREVIOUSLY VALIDATED?                  
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   THE KEY HAS CHANGED                          
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VK010               YES                                          
         CLI   ACTNUM,ACTADD       CREATING A NEW DEAL?                         
         BE    VK020                                                            
         B     ERRMIS              BUILD THE KEY FOR GENCON                     
*                                                                               
VK003    LA    R2,DLLREPH                                                       
         XC    SVLREP,SVLREP                                                    
         CLI   5(R2),0                                                          
         BE    VK006                                                            
         GOTO1 VRCPACK,DMCB,(C'P',8(R2)),SVLREP                                 
*                                                                               
VK006    XC    KEY,KEY                                                          
         LA    R6,KEY              REQUIRED                                     
         USING SDLKEY,R6                                                        
         MVI   SDLKTYPE,SDLKTYPQ   X'0D'                                        
         MVI   SDLKSTYP,SDLKSTYQ   X'0E'                                        
         MVC   SDLKAM,SVAGMD                                                    
*                                                                               
         OI    DLLMEDH+4,X'20'     SET PREVIOUSLY VALIDATED BIT                 
         OI    DLLMEDH+6,X'80'                                                  
         CLI   ACTNUM,ACTREP       REPORT SCREEN ONLY HAS MEDIA FIELD           
         BE    VKXX                                                             
         OI    DLLREPH+4,X'20'                                                  
         OI    DLLREPH+6,X'80'                                                  
         B     VKXX                                                             
*                                                                               
VK010    CLI   ACTNUM,ACTADD       ADDING ONE?                                  
         BE    VK020                                                            
*                                                                               
         LLC   R1,5(R2)            SAVE OUR DEAL NUMBER                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,SVDNO                                                       
         XC    SVDNO,=X'FFFF'                                                   
         B     VK030               SKIP MANUAL CHECK                            
*                                                                               
VK020    XC    KEY,KEY                                                          
         LA    R6,KEY              REQUIRED                                     
         USING SDLKEY,R6                                                        
         MVI   SDLKTYPE,SDLKTYPQ   X'0D'                                        
         MVI   SDLKSTYP,SDLKSTYQ   X'0E'                                        
         MVC   SDLKAM,BAGYMD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(SDLKDLNO-SDLKEY),KEYSAVE                                     
         BE    VK023                                                            
         LA    R1,1                                                             
         B     VK026                                                            
*                                                                               
VK023    MVC   SVDNO,SDLKDLNO                                                   
         XC    SVDNO,=X'FFFF'                                                   
         XR    R1,R1                                                            
         ICM   R1,3,SVDNO                                                       
         LA    R1,1(R1)                                                         
VK026    STCM  R1,3,SVDNO                                                       
         XC    8(L'DLMDNO,R2),8(R2)  REDISPLAY THE DEAL NO                      
         OI    MISCFLG1,MF1KYCHG                                                
         OI    6(R2),X'80'                                                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(L'DLMDNO,R2),DUB                                               
         XC    SVDNO,=X'FFFF'                                                   
*                                                                               
VK030    TM    MISCFLG1,MF1KYCHG   THE KEY HAS CHANGED                          
         BZ    VK040                                                            
         MVI   CURRSTPG,0                                                       
         MVI   PAGEUPDN,0                                                       
*                                                                               
VK040    XC    KEY,KEY                                                          
         LA    R6,KEY              REQUIRED                                     
         USING SDLKEY,R6                                                        
         MVI   SDLKTYPE,SDLKTYPQ   X'0D'                                        
         MVI   SDLKSTYP,SDLKSTYQ   X'0E'                                        
         MVC   SDLKAM,BAGYMD                                                    
         MVC   SDLKDLNO,SVDNO                                                   
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VKX                                                              
         GOTO1 HIGH                                                             
         CLC   KEY(SDLKSTDT-SDLKEY),KEYSAVE   SAME DEAL NUMBER?                 
         BE    VKX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              REQUIRED                                     
         USING SDLKEY,R6                                                        
         MVI   SDLKTYPE,SDLKTYPQ   X'0D'                                        
         MVI   SDLKSTYP,SDLKSTYQ   X'0E'                                        
         MVC   SDLKAM,BAGYMD                                                    
         MVC   SDLKDLNO,SVDNO                                                   
*                                                                               
VKX      MVC   SVSDLKEY(L'SDLKEY),KEY    BACK UP THE KEY                        
VKXX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
         CLI   PFKEY,0             USER PAGING?                                 
         BNE   DR                                                               
*                                                                               
         L     R6,AIO                                                           
         USING SDEALRCD,R6                                                      
*                                                                               
         XC    PRVIDELM,PRVIDELM                                                
         XC    PRMIDELM,PRMIDELM                                                
         LA    R4,PRMIDELM                                                      
         USING SDLIDELD,R4                                                      
*                                                                               
         CLI   ACTNUM,ACTADD       ARE WE ADDING?                               
         BNE   VR010                                                            
         MVC   SDLRLEN,=AL2(24)                                                 
         MVC   SDLRAGY,AGENCY                                                   
         MVI   SDLIDEL,SDLIDELQ    X'10'                                        
         MVI   SDLIDLEN,SDLIDLNQ                                                
         B     VR020                                                            
*                                                                               
VR010    MVI   ELCODE,SDLIDELQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRVIDELM(SDLIDLNQ),0(R6)                                         
         MVC   PRMIDELM(SDLIDLNQ),0(R6)                                         
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)  DELETE WHAT WE FOUND            
*******                                                                         
* DEAL NAME                                                                     
*******                                                                         
VR020    LA    R2,DLMDNMH                                                       
         CLI   5(R2),0             WE MUST HAVE A DEAL NAME                     
         BE    ERRMIS                                                           
         MVC   SDLIDNAM,8(R2)                                                   
         OC    SDLIDNAM,SPACES                                                  
*******                                                                         
* DEAL PERIOD                                                                   
*******                                                                         
         LA    R2,DLMPRDH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(X'20',PERVALST)                       
         TM    4(R1),X'03'         ANY ERRORS?                                  
         BNZ   ERRINV                                                           
*                                                                               
PVALD    USING PERVALD,PERVALST                                                 
         CLC   PVALD.PVALCEND,PVALD.PVALCSTA                                    
         BL    ERRINV                                                           
         MVC   SDLIDSDT,PVALD.PVALCSTA                                          
         MVC   SDLIDEDT,PVALD.PVALCEND                                          
* ADD 2 YEARS TO THE START DATE AND MAKE SURE END IS LESS THAN THAT             
         GOTO1 ADDAY,DMCB,(C'Y',PVALD.PVALESTA),WORK,2                          
         CLC   PVALD.PVALEEND,WORK                                              
         BNL   ERGRT24M                                                         
*                                                                               
         TM    MISCFLG1,MF1KYCHG   IF KEY CHANGED                               
         BNZ   *+12                THEN WE HAVE TO VALIDATE                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR030               YES                                          
         XC    KEY,KEY             CHECK FOR INTERSECTING DATES                 
         LA    R6,KEY                                                           
         USING SDLKEY,R6                                                        
         MVI   SDLKTYPE,SDLKTYPQ   X'0D'                                        
         MVI   SDLKSTYP,SDLKSTYQ   X'0E'                                        
         MVC   SDLKAM,BAGYMD                                                    
         GOTO1 HIGH                                                             
*                                                                               
VR022    CLC   KEY(SDLKDLNO-SDLKEY),KEYSAVE   SAME AGENCY MEDIA?                
         BNE   VR030                          NOT INTERSECTING OTHERS           
         CLC   KEY(SDLKSTDT-SDLKEY),SVSDLKEY  FOUND DEAL I'M CHANGING?          
         BE    VR026                          YES, SKIP IT                      
         CLC   PVALD.PVALCSTA,SDLKNDDT                                          
         BH    VR026                                                            
         CLC   PVALD.PVALCEND,SDLKSTDT                                          
         BL    VR026                                                            
*                                                                               
         MVI   BLOCK,6                      L'LENGTH + L'DEAL NUMBER            
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),SDLKDLNO                                               
         XC    FULL+2(2),=X'FFFF'                                               
         EDIT  (B4,FULL),(5,BLOCK+1),FILL=0                                     
         MVI   BLOCK+1+5,0                  TERMINATING 0                       
         B     ERINTRSC            DATES INTERECT WITH DATES IN DEAL &T         
*                                                                               
VR026    GOTO1 SEQ                                                              
         B     VR022                                                            
         DROP  R6,PVALD                                                         
*******                                                                         
* REP CODE                                                                      
*******                                                                         
VR030    LA    R2,DLMREPH                                                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         OC    8(3,R2),SPACES                                                   
         GOTO1 VRCPACK,DMCB,(C'P',8(R2)),SDLIDREP                               
*                                                                               
         TM    MISCFLG1,MF1KYCHG   IF KEY CHANGED                               
         BNZ   *+12                THEN WE HAVE TO VALIDATE                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   VR040               YES                                          
         XC    KEY,KEY             CHECK FOR INTERSECTING DATES                 
         LA    R6,KEY                                                           
         USING SDLKEY,R6                                                        
         MVI   SDLKTYPE,SDLKTYPQ   X'0D'                                        
         MVI   SDLKSTYP,SDLKSTYQ   X'0E'                                        
         MVC   SDLKAM,BAGYMD                                                    
         GOTO1 HIGH                                                             
*                                                                               
VR032    CLC   KEY(SDLKDLNO-SDLKEY),KEYSAVE   SAME AGENCY MEDIA?                
         BNE   VR040                          NOT INTERSECTING OTHERS           
         CLC   KEY(SDLKSTDT-SDLKEY),SVSDLKEY  FOUND DEAL I'M CHANGING?          
         BE    VR036                          YES, SKIP IT                      
         CLC   SDLIDREP,SDLKREP                                                 
         BNE   VR036                                                            
         MVI   BLOCK,6                      L'LENGTH + L'DEAL NUMBER            
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),SDLKDLNO                                               
         XC    FULL+2(2),=X'FFFF'                                               
         EDIT  (B4,FULL),(5,BLOCK+1),FILL=0                                     
         MVI   BLOCK+1+5,0                  TERMINATING 0                       
         B     ERSAMREP            REP IS ALREADY USED IN DEAL &T               
*                                                                               
VR036    GOTO1 SEQ                                                              
         B     VR032                                                            
         DROP  R6                                                               
*                                                                               
VR040    L     R6,AIO                                                           
         USING SDEALRCD,R6                                                      
         MVC   SDLKSTDT,SDLIDSDT   CHANGE KEY OF REC EVEN IF NOT ADD            
         MVC   SDLKNDDT,SDLIDEDT      AS WE'LL HAVE TO ADD NEW ACTIVE           
         MVC   SDLKREP,SDLIDREP       KEY IF THERE IS A CHANGE                  
*                                                                               
         LA    R3,SDLRFRST                                                      
         GOTO1 RECUP,DMCB,(C'S',AIO),PRMIDELM,(R3)                              
         DROP  R4                                                               
***************                                                                 
* HANDLE THE STATIONS                                                           
***************                                                                 
VR100    L     R6,AIO                                                           
         USING SDEALRCD,R6                                                      
         LA    R5,SDLRFRST         FIND STATION ELEM FOR THE PAGE               
VR110    CLI   0(R5),0             END OF RECORD?                               
         BE    VR130               THEN WE'RE INSERTING                         
         CLI   0(R5),SDLSTELQ      X'20' - STATION ELEM?                        
         BE    VR115                                                            
         LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     VR110                                                            
*                                                                               
VR115    MVI   STACOUNT,0                                                       
         CLI   CURRSTPG,0          ARE WE ON THE VERY FIRST PAGE?               
         BE    VR130               YES                                          
         LLC   R0,CURRSTPG                                                      
         MHI   R0,13               14 LINES PER PAGE, BUT NEXT PAGE             
         STC   R0,STACOUNT                                                      
         MHI   R0,SDLSTLNQ             REPEATS LAST LINE OF PREV PAGE           
         AR    R5,R0                                                            
*                                                                               
VR130    DS    0H                                                               
***********************************                                             
* R5 = EOR OR THE A(1ST STATION OF THE PAGE)                                    
***********************************                                             
VR140    LA    R3,DLML1STH                                                      
         USING STALIND,R3                                                       
VR150    LA    R2,STALSTAH                                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VR155                                                            
         LA    RE,DLML1STH         ARE WE ON THE 1ST LINE?                      
         CR    R2,RE                                                            
         BNE   VR155                                                            
         CLI   0(R5),SDLSTELQ      YES                                          
         BE    ERRMVLIN                                                         
*                                                                               
VR155    CLI   STACOUNT,75         LIMIT OF 75 STATIONS PER DEAL RECORD         
         BNL   ER2MNYST                                                         
*                                                                               
         XC    SVSTELEM,SVSTELEM                                                
         LA    R4,SVSTELEM                                                      
         USING SDLSTELD,R4                                                      
         MVI   SDLSTEL,SDLSTELQ    X'20'                                        
         MVI   SDLSTLEN,SDLSTLNQ                                                
*                                                                               
         MVI   USEIONUM,3                                                       
         GOTO1 VALISTA                                                          
         MVC   AIO,AIO1                                                         
         MVC   SDLSTSTA,BSTA                                                    
***************                                                                 
* CHECK IF WE ALREADY HAVE THIS STATION IN THE RECORD                           
***************                                                                 
         L     RE,AIO                                                           
         AHI   RE,SDLRFRST-SDLKEY                                               
VR160    CLI   0(RE),0             END OF RECORD?                               
         BE    VR170               THEN STATION IS NOT A DUPLICATE              
         CLI   0(RE),SDLSTELQ                                                   
         BNE   VR165                                                            
         CLC   SDLSTSTA-SDLSTELD(L'SDLSTSTA,RE),SDLSTSTA                        
         BNE   VR165                                                            
         CR    RE,R5               MODIFYING THIS SAME ELEM?                    
         BE    VR165                                                            
         B     ERRDUPLC            NO, WE HAVE A DUPLICATE STATION              
VR165    LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     VR160                                                            
*                                                                               
VR170    LA    R2,STALGRSH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         LLC   RE,5(R2)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(C'N',8(R2)),,0    INTEGER                          
         MVC   SDLSTGRS,DMCB+4                                                  
*                                                                               
         LA    R2,STALNETH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         LLC   RE,5(R2)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(C'N',8(R2)),,0    INTEGER                          
         MVC   SDLSTNET,DMCB+4                                                  
*                                                                               
         LA    R2,STALPCTH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         LLC   RE,5(R2)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(C'N',8(R2)),,0    INTEGER                          
         L     RE,DMCB+4                                                        
         CHI   RE,100              CAN'T BE > 100%                              
         BH    ERGRTHUN            ERROR                                        
         STC   RE,SDLSTCCM         CASH COMMITMENT %-AGE                        
*                                                                               
         LA    R2,STALBYRH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         OC    STALBYR,SPACES                                                   
         MVC   SDLSTBYR,8(R2)                                                   
         DROP  R4                                                               
*                                                                               
         CLI   0(R5),SDLSTELQ      X'20' - WE HAVE STA ELEM FOR THIS?           
         BNE   VR180                                                            
         MVC   0(SDLSTLNQ,R5),SVSTELEM    YES, OVERWRITE IT THEN                
         B     VR190                                                            
*                                                                               
VR180    GOTO1 RECUP,DMCB,(C'S',AIO),SVSTELEM,(R5)                              
*                                                                               
VR190    LLC   R0,1(R5)            R5 = A(NEXT STATION ELEM)                    
         AR    R5,R0                                                            
         LA    R3,STALNXTL                                                      
*                                                                               
         LLC   R0,STACOUNT         SO WE KNOW HOW MANY STATIONS                 
         AHI   R0,1                                                             
         STC   R0,STACOUNT                                                      
*                                                                               
         LA    R0,DLMLLSTH                                                      
         CR    R3,R0                                                            
         BH    VRX                 DONE WITH ALL LINES FOR THIS SCREEN?         
         LA    R2,STALSTAH                                                      
         CLI   5(R2),0             ANY STATION INPUT ON THIS LINE?              
         BNE   VR150               YES, THEN GO VALIDATE IT                     
         CLI   0(R5),SDLSTELQ      NO, BUT ALREADY A STA FOR THIS LINE?         
         BE    ERRMVLIN            YES, THEN A STATION MUST REMAIN              
*                                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
         L     R6,AIO                                                           
         USING SDEALRCD,R6                                                      
         MVC   SVSDLKEY(13),0(R6)                                               
         MVC   BYTE,SDLKAM         CONVERT BAGYMD INTO 1 CHAR MEDIA             
         NI    BYTE,X'0F'          TURN OFF AGENCY PORTION                      
*                                                                               
         LA    R5,MEDTAB                                                        
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   DK10                                                             
*                                                                               
DK20     MVC   DLMMED(1),0(R5)                                                  
         OI    DLMMEDH+4,X'20'                                                  
         OI    DLMMEDH+6,X'80'                                                  
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF,SDLKDLNO                                                    
         XC    HALF,=X'FFFF'                                                    
         XR    R1,R1                                                            
         ICM   R1,3,HALF                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DLMDNO,DUB                                                       
         OI    DLMDNOH+4,X'20'                                                  
         OI    DLMDNOH+6,X'80'                                                  
         MVI   CURRSTPG,0                                                       
         MVI   PAGEUPDN,0                                                       
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
         OI    DLMMEDH+4,X'20'     MARK THE KEY FIELDS AS PREVIOUSLY            
         OI    DLMMEDH+6,X'80'        VALIDATED                                 
         OI    DLMDNOH+4,X'20'                                                  
         OI    DLMDNOH+6,X'80'                                                  
*                                                                               
         CLI   CALLSP,0            CALLED FROM ANOTHER?                         
         BE    DR001                                                            
         MVC   DLMPFKY+18(11),=C'PF12=Return'                                   
         OI    DLMPFKYH+6,X'80'                                                 
*                                                                               
DR001    CLI   PAGEUPDN,C'-'       PAGE UP?                                     
         BNE   DR003                                                            
         CLI   CURRSTPG,0          ARE WE ON THE 1ST PAGE ALREADY?              
         BE    DR009               YES, NOTHING TO DO                           
         LLC   R0,CURRSTPG                                                      
         SHI   R0,1                                                             
         STC   R0,CURRSTPG                                                      
         B     DR009                                                            
*                                                                               
DR003    CLI   PAGEUPDN,C'+'       PAGE DOWN?                                   
         BNE   DR006                                                            
         CLI   CURRSTPG,5          MAXIMUM OF 5 PAGES                           
         BE    SETUP50             YES, NOHTING TO DO                           
         LLC   R0,CURRSTPG                                                      
         AHI   R0,1                                                             
         STC   R0,CURRSTPG                                                      
         B     DR009                                                            
*                                                                               
DR006    DS    0H                                                               
*                                                                               
DR009    MVI   PAGEUPDN,0                                                       
*                                                                               
         L     R6,AIO                                                           
         USING SDEALRCD,R6                                                      
         LA    R6,SDLRFRST                                                      
DR010    CLI   0(R6),0                                                          
         BE    DRX                                                              
         CLI   0(R6),SDLIDELQ      X'10' - PRIMARY ELEM?                        
         BE    DR020                                                            
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR010                                                            
***********************************                                             
* PRIMARY ELEMENT                                                               
***********************************                                             
         USING SDLIDELD,R6                                                      
DR020    XC    DLMDNM,DLMDNM                                                    
         MVC   DLMDNM,SDLIDNAM                                                  
         OI    DLMDNMH+4,X'20'                                                  
         OI    DLMDNMH+6,X'80'                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(X'12',SDLIDSDT),(5,DLMPRD)                          
         OI    DLMPRDH+4,X'20'                                                  
         OI    DLMPRDH+6,X'80'                                                  
*                                                                               
         GOTO1 VRCPACK,DMCB,(C'U',SDLIDREP),DLMREP                              
         OI    DLMREPH+4,X'20'                                                  
         OI    DLMREPH+6,X'80'                                                  
***********************************                                             
* STATION ELEMENT                                                               
***********************************                                             
DR030    TWAXC DLML1STH,DLMCDTEH   DO WE NEED A MODIFIED TWAXC?                 
         L     R6,AIO                                                           
         USING SDEALRCD,R6                                                      
         LA    R6,SDLRFRST         FIND STATION ELEM FOR THE PAGE               
DR035    CLI   0(R6),0             END OF RECORD?                               
         BE    DR090                                                            
         CLI   0(R6),X'F1'           OR  ACTIVITY ELEM?                         
         BE    DR090               THEN WE'RE DONE                              
         CLI   0(R6),SDLSTELQ      X'20' - STATION ELEM?                        
         BE    DR040                                                            
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR035                                                            
*                                                                               
DR040    CLI   CURRSTPG,0          ARE WE ON THE VERY FIRST PAGE?               
         BE    DR050               YES                                          
         LLC   R0,CURRSTPG                                                      
         MHI   R0,13               14 LINES PER PAGE, BUT NEXT PAGE             
         MHI   R0,SDLSTLNQ             REPEATS LAST LINE OF PREV PAGE           
         AR    R6,R0                                                            
*                                                                               
         USING SDLSTELD,R6                                                      
DR050    LA    R3,DLML1STH                                                      
         USING STALIND,R3                                                       
DR060    CLI   0(R6),0             END OF RECORD?                               
         BE    DR090                                                            
         CLI   0(R6),SDLSTELQ      OR NO LONGER A STATION ELEM?                 
         BNE   DR090               YES, DONE WITH DISPLAY OF STATIONS           
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),SDLSTSTA                                               
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,STALSTA                                  
         EDIT  (B4,SDLSTGRS),(9,STALGRS),ALIGN=LEFT                             
         EDIT  (B4,SDLSTNET),(9,STALNET),ALIGN=LEFT                             
         EDIT  (B1,SDLSTCCM),(3,STALPCT),FILL=0                                 
         MVC   STALBYR,SDLSTBYR                                                 
*                                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
*                                                                               
         LA    R3,STALNXTL         BUMP TO NEXT LINE FOR NEXT STATION           
         LA    R0,DLMLLSTH                                                      
         CR    R3,R0                                                            
         BNH   DR060               SHOW ACTIVITY INFO                           
***********************************                                             
* ACTIVITY ELEM                                                                 
***********************************                                             
DR090    DS    0H                                                               
         XC    DLMCDTE,DLMCDTE                                                  
         OI    DLMCDTEH+6,X'80'                                                 
         XC    DLMCRTR,DLMCRTR                                                  
         OI    DLMCRTRH+6,X'80'                                                 
*                                                                               
         XC    DLMADTE,DLMADTE                                                  
         OI    DLMADTEH+6,X'80'                                                 
         XC    DLMCWHO,DLMCWHO                                                  
         OI    DLMCWHOH+6,X'80'                                                 
*                                                                               
DR095    CLI   0(R6),0                                                          
         BE    DRX                                                              
         CLI   0(R6),X'F1'                                                      
         BE    DR100                                                            
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR095                                                            
*                                                                               
         USING ACTVD,R6                                                         
DR100    GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,DLMCDTE)                             
         GOTO1 SECMGRID,DMCB,DLMCRTRH,ACTVADID                                  
*                                                                               
         OC    ACTVCHDT,ACTVCHDT                                                
         BZ    DR110                                                            
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,DLMADTE)                             
*                                                                               
DR110    OC    ACTVCHID,ACTVCHID                                                
         BZ    DRX                                                              
         GOTO1 SECMGRID,DMCB,DLMCWHOH,ACTVCHID                                  
*                                                                               
DRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        SECMGRID SUBROUTINE                                          *         
*        P1 = SCREEN HEADER                                           *         
*        P2 = PID                                                     *         
***********************************************************************         
SECMGRID NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
* PERSONAL ID                                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SECALPHA                                                 
         MVC   PASSWD,0(R4)                                                     
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK   GET PERSONAL ID                             
         MVC   AIO,AIO1                                                         
         MVC   8(8,R2),PRSNLID                                                  
         MVI   5(R2),8             INPUT LENGTH OF 8                            
         OI    6(R2),X'80'         RETRANSMIT                                   
         DROP  R3                                                               
*                                                                               
SMIDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LIST DEAL RECORDS                                                             
***********************************************************************         
XRCPUT   DS    0H                                                               
*  DID THE DEAL PERIOD OR THE REP CHANGE?                                       
         OC    PRVIDELM,PRVIDELM   DID WE HAVE A PREVIOUS ID ELEM?              
         BZ    XRPT050             NO, SO MUST BE ADDING                        
         CLC   PRVIDELM+SDLIDSDT-SDLIDELD(6),PRMIDELM+SDLIDSDT-SDLIDELD         
         BE    XRPT050                                                          
*                                                                               
         XC    KEY,KEY                                                          
         L     R6,AIO                                                           
         MVC   KEY(L'SDLKEY),0(R6)   WE'LL NEED TO ADD A NEW ACTIVE KEY         
         OI    DMINBTS,X'88'       READ FOR UPDATE AND DELETED                  
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(L'SDLKEY),KEYSAVE  DID THIS KEY EXIST PREVIOUSLY?            
         BNE   XRPT010                NO, HAVE TO ADD                           
         NI    KEY+SDLKSTAT-SDLKEY,X'7F'  REMOVE DELETED BIT                    
         MVC   KEY+SDLKDA-SDLKEY(L'SDLKDA),GLOBDA                               
         GOTO1 WRITE                                                            
         B     XRPT020             NOW DELETE THE OLD ACTIVE KEY                
*                                                                               
XRPT010  XC    KEY,KEY                                                          
         MVC   KEY(L'SDLKEY),0(R6)   WE'LL NEED TO ADD A NEW ACTIVE KEY         
         MVC   KEY+SDLKDA-SDLKEY(L'SDLKDA),GLOBDA                               
         GOTO1 ADD                                                              
*                                                                               
XRPT020  XC    KEY,KEY                                                          
         MVC   KEY(L'SDLKEY),0(R6)    SETUP OLD ACTIVE KEY TO DELETE            
         MVC   KEY+SDLKSTDT-SDLKEY(6),PRVIDELM+SDLIDSDT-SDLIDELD                
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 READ                                                             
         OI    KEY+SDLKSTAT-SDLKEY,X'80'  SET DELETED BIT                       
         GOTO1 WRITE                                                            
*                                                                               
XRPT050  CLI   PAGEUPDN,0          AND WE HAVE PAGING?                          
         BNE   DR                  YES, THEN PAGE                               
         B     XIT                                                              
***********************************************************************         
* LIST DEAL RECORDS                                                             
***********************************************************************         
LR       DS    0H                                                               
         MVI   NLISTS,18                                                        
         TM    MISCFLG1,MF1KYCHG   1ST TIME IN THE LIST?                        
         BNZ   LR005                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(L'SDLKEY),SVSDLKEY                                           
         B     LRHIGH                                                           
*                                                                               
LR005    XC    KEY,KEY                                                          
         LA    R6,KEY              REQUIRED                                     
         USING SDLKEY,R6                                                        
         MVI   SDLKTYPE,SDLKTYPQ   X'0D'                                        
         MVI   SDLKSTYP,SDLKSTYQ   X'0E'                                        
         MVC   SDLKAM,SVAGMD                                                    
*                                                                               
LRHIGH   GOTO1 HIGH                                                             
         B     LR010                                                            
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR010    CLC   KEY(SDLKAM-SDLKEY),KEYSAVE   STILL A DEAL RECORD?                
         BNE   LRX                                                              
*                                                                               
LR013    TM    SVAGMD,X'0F'        USER PUT IN A MEDIA FILTER?                  
         BNZ   LR015                                                            
         CLC   SDLKAM,SVAGMDLO     NO, GO THRU ALL MEDIAS FOR THE AGY           
         BL    LRSEQ                                                            
         CLC   SDLKAM,SVAGMDHI                                                  
         BH    LRX                 RESTART THE LIST NEXT TIME                   
         B     LR020                                                            
*                                                                               
LR015    CLC   SDLKAM,SVAGMD                                                    
         BNE   LRX                                                              
*                                                                               
LR020    OC    SVLREP,SVLREP       ANY FILTERING REP CODE?                      
         BZ    LR030                                                            
         CLC   SDLKREP,SVLREP      YES, DOES IT MATCH?                          
         BNE   LRSEQ                                                            
*                                                                               
LR030    GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   SVSDLKEY,SDLKEY                                                  
*                                                                               
         MVC   LISTAR,SPACES                                                    
LRDSCT   USING LSTDLNO,LISTAR                                                   
         MVC   BYTE,SDLKAM         CONVERT BAGYMD INTO 1 CHAR MEDIA             
         NI    BYTE,X'0F'          TURN OFF AGENCY PORTION                      
*                                                                               
         LA    R5,MEDTAB                                                        
LR035    CLC   BYTE,1(R5)                                                       
         BE    LR040                                                            
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   LR035                                                            
*                                                                               
LR040    MVC   LRDSCT.LSTMEDIA,0(R5)                                            
         XC    SDLKDLNO,=X'FFFF'                                                
         EDIT  (B2,SDLKDLNO),(5,LRDSCT.LSTDLNO),ALIGN=LEFT                      
         LA    R6,SDLRFRST                                                      
         USING SDLIDELD,R6                                                      
         MVC   LRDSCT.LSTDLNM,SDLIDNAM                                          
         GOTO1 DATCON,DMCB,(X'12',SDLIDSDT),(5,LRDSCT.LSTPRIOD)                 
         GOTO1 VRCPACK,DMCB,(C'U',SDLIDREP),LRDSCT.LSTREPCD                     
         GOTO1 LISTMON                                                          
*                                                                               
         LA    R6,KEY                                                           
         B     LRSEQ                                                            
         DROP  LRDSCT                                                           
*                                                                               
LRX      NI    DLLMEDH+4,X'FF'-X'20'  TAKE OFF PREV VALIDATED BIT               
         OI    DLLMEDH+6,X'80'                                                  
         B     XIT                                                              
***********************************************************************         
* PRINT DEAL RECORDS                                                            
***********************************************************************         
PR       LA    R1,HEDSPECS         INITIALIZE                                   
         ST    R1,SPECS                                                         
*****    LA    R1,HOOK                                                          
*****    ST    R1,HEADHOOK                                                      
*****    MVI   HDHOOKOK,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              REQUIRED                                     
         USING SDLKEY,R6                                                        
         MVI   SDLKTYPE,SDLKTYPQ   X'0D'                                        
         MVI   SDLKSTYP,SDLKSTYQ   X'0E'                                        
         MVC   SDLKAM,SVAGMD                                                    
*                                                                               
         GOTO1 HIGH                                                             
         B     PR010                                                            
*                                                                               
PRSEQ    GOTO1 SEQ                                                              
*                                                                               
PR010    CLC   KEY(SDLKAM-SDLKEY),KEYSAVE   STILL A DEAL RECORD?                
         BNE   PRX                                                              
*                                                                               
         TM    SVAGMD,X'0F'        USER PUT IN A MEDIA FILTER?                  
         BNZ   PR015                                                            
         CLC   SDLKAM,SVAGMDLO     NO, GO THRU ALL MEDIAS FOR THE AGY           
         BL    PRSEQ                                                            
         CLC   SDLKAM,SVAGMDHI                                                  
         BH    PRX                 RESTART THE LIST NEXT TIME                   
         B     PR020                                                            
*                                                                               
PR015    CLC   SDLKAM,SVAGMD                                                    
         BNE   PRX                                                              
*                                                                               
PR020    GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   SVSDLKEY,SDLKEY                                                  
*                                                                               
         LA    R4,P                                                             
         USING PRTLIND,R4                                                       
         MVC   BYTE,SDLKAM         CONVERT BAGYMD INTO 1 CHAR MEDIA             
         NI    BYTE,X'0F'          TURN OFF AGENCY PORTION                      
*                                                                               
         LA    R5,MEDTAB                                                        
PR025    CLC   BYTE,1(R5)                                                       
         BE    PR030                                                            
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         CLI   0(R5),X'FF'         END OF TABLE?                                
         BNE   PR025                                                            
*                                                                               
PR030    MVC   PRTMEDIA+1(1),0(R5)                                              
*                                                                               
         XC    SDLKDLNO,=X'FFFF'                                                
         EDIT  (B2,SDLKDLNO),(5,PRTDLNO),FILL=0                                 
         LA    R6,SDLRFRST                                                      
         USING SDLIDELD,R6                                                      
         MVC   PRTDLNM,SDLIDNAM                                                 
         GOTO1 DATCON,DMCB,(X'12',SDLIDSDT),(5,PRTPRIOD)                        
         GOTO1 VRCPACK,DMCB,(C'U',SDLIDREP),PRTREPCD                            
***************                                                                 
* USING AIO3 TO STORE STATIONS TO SORT. EACH STATION WILL BE 5 BYTES            
*        LONG IN THIS IOAREA. R3 WILL CONTAIN NUMBER OF STATIONS.               
***************                                                                 
         L     RE,AIO3                                                          
         LA    RF,4000             LIOS CURRENTLY IS 6000 BYTES                 
         XCEFL                                                                  
*                                                                               
         L     R5,AIO3                                                          
         XR    R3,R3                                                            
*                                                                               
PR040    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    PR050               EOR, READY TO SORT STATIONS                  
         CLI   0(R6),SDLSTELQ      X'20' - STATION ELEM?                        
         BNE   PR040               STATIONS ARE THE ONLY THING LEFT             
*                                                                               
         USING SDLSTELD,R6                                                      
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'SDLSTSTA),SDLSTSTA                                      
         GOTO1 MSUNPK,DMCB,WORK,WORK+10,WORK+20                                 
*                                                                               
         LA    R3,1(R3)                                                         
         MVC   0(5,R5),WORK+20                                                  
         LA    R5,5(R5)                                                         
         B     PR040                                                            
*                                                                               
PR050    LTR   R3,R3               RECORD SHOULD REALLY HAVE 1 STATION          
         BZ    PR090               BUT NO REASON TO KILL THE REPORT             
*                                                                               
         GOTO1 XSORT,DMCB,(0,AIO3),(R3),5,5,0                                   
*                                                                               
         L     R6,AIO3                                                          
         LA    R5,PRTSTALS                                                      
         LA    R0,PRTLINX                                                       
*                                                                               
PR060    LR    RF,R5               MAKE SURE WE HAVE ENUF ROOM ON LINE          
         LA    RF,5(RF)               FOR THE NEXT STATION                      
         CR    RF,R0                                                            
         BL    PR063                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,PRTSTALS                                                      
*                                                                               
PR063    MVC   0(5,R5),0(R6)       MOVE ONE STATION OVER                        
         LA    R5,4(R5)                                                         
PR066    CLI   0(R5),C' '                                                       
         BH    PR070                                                            
         BCTR  R5,0                                                             
         B     PR066                                                            
*                                                                               
PR070    LA    R5,1(R5)            ALSO A COMMA                                 
         MVI   0(R5),C','                                                       
         LA    R5,1(R5)                                                         
*                                                                               
         LA    R6,5(R6)            POINT TO THE NEXT STATION                    
         BCT   R3,PR060                                                         
*                                                                               
PR090    BCTR  R5,0                                                             
         MVI   0(R5),C' '                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R6,KEY                                                           
         B     PRSEQ                                                            
         DROP  R4                                                               
*                                                                               
PRX      B     XIT                                                              
*                                                                               
HEDSPECS SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,PAGE                                                        
         SSPEC H1,52,C' DEAL LIST REPORT '                                      
         SSPEC H2,52,C' ---------------- '                                      
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H3,95,REPORT                                                     
         SSPEC H3,105,RUN                                                       
*                                                                               
         SSPEC H5,1,C'Deal Number/Name                     Deal Perio'          
         SSPEC H5,48,C'd       Med Rep Stations'                                
         SSPEC H6,1,C'------------------------------------ ----------'          
         SSPEC H6,48,C'------- --- --- ------------------------------'          
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         J     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRSEC   MVI   ERROR,SECLOCK                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRCGRP  MVC   ERRNUM,=AL2(CLTINGRP)                                            
         B     SPERREX                                                          
ERRPRDX  MVC   ERRNUM,=AL2(PRDEXIST)                                            
         B     SPERREX                                                          
ERRDADD  MVC   ERRNUM,=AL2(NODELADD)                                            
         B     SPERREX                                                          
ERRCORD  MVC   ERRNUM,=AL2(NOCOORD)                                             
         B     SPERREX                                                          
ERRTCP   MVC   ERRNUM,=AL2(NOTCLPD)                                             
         B     SPERREX                                                          
ERRTCLT  MVC   ERRNUM,=AL2(INVTCLT)                                             
         B     SPERREX                                                          
ERRTCLQ  MVC   ERRNUM,=AL2(INVTCLTQ)                                            
         B     SPERREX                                                          
ERRCTCLQ MVC   ERRNUM,=AL2(CHATCLT)                                             
         B     SPERREX                                                          
ERRCTPRD MVC   ERRNUM,=AL2(CHATPRD)                                             
         B     SPERREX                                                          
ERROFC   MVC   ERRNUM,=AL2(INVOFC)                                              
         B     SPERREX                                                          
ERR1OFC  MVC   ERRNUM,=AL2(OFCONE)                                              
         B     SPERREX                                                          
ERR2OFC  MVC   ERRNUM,=AL2(OFCTWO)                                              
         B     SPERREX                                                          
ERRAAGY  MVC   ERRNUM,=AL2(INVAAGY)                                             
         B     SPERREX                                                          
ERRSYS   MVC   ERRNUM,=AL2(SYSNOPEN)                                            
         B     SPERREX                                                          
ERRSWS   MVC   ERRNUM,=AL2(SYSWS)                                               
         B     SPERREX                                                          
ERRCMP   MVC   ERRNUM,=AL2(NOACCCMP)                                            
         B     SPERREX                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRNOBRD MVC   ERRNUM,=AL2(NOBRD)                                               
         B     SPERREX                                                          
ERRMEDRX MVC   ERRNUM,=AL2(MEDRX)                                               
         B     SPERREX                                                          
ERRDUPLC MVC   ERRNUM,=AL2(DUPSTATN)                                            
         B     SPERREX                                                          
ERGRTHUN MVC   ERRNUM,=AL2(SURPHUND) INPUT %-AGE MAY NOT SURPASS 100.00         
         B     SPERREX                                                          
ERRMVLIN MVC   ERRNUM,=AL2(RMVDLLIN) CANNOT REMOVE LINES FROM A DEAL            
         B     SPERREX                                                          
ERGRT24M MVC   ERRNUM,=AL2(DLPRD24M) PERIOD CANNOT SPAN MORE 24 MONTHS          
         B     SPERREX                                                          
ER2MNYST MVC   ERRNUM,=AL2(TOOMNYST) LIMIT OF 75 STA'S PER DEAL RECORD          
         B     SPERREX                                                          
*                                                                               
ERINTRSC MVC   ERRNUM,=AL2(INTRSCDT)                                            
         B     ERRRTEXT                                                         
ERSAMREP MVC   ERRNUM,=AL2(SAMEREP)                                             
         B     ERRRTEXT                                                         
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
ERRRTEXT OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   GTLTXT,BLOCK        LENGTH OF INSERTION TEXT                     
         LA    RE,BLOCK+1                                                       
         STCM  RE,7,GTATXT         A(INSERTION TEXT)                            
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         B     VSFMERR                                                          
         DROP  RF                                                               
*                                  SHORT DESP OF ERROR MSGS                     
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
CLTINGRP EQU   387                 CLIENT STILL EXIST IN GROUP                  
PRDEXIST EQU   526                 PRODUCT STILL EXIST FOR THIS CLIENT          
NODELADD EQU   525                 CAN'T DELETE ON ADD                          
NOCOORD  EQU   1075                AGENCY DOES NOT AUTHORIZE COORD.             
NOTCLPD  EQU   517                 NO TRAFFIC CLT/SEQ & PRD ALLOWED             
INVTCLT  EQU   518                 INVALID TRAFFIC CLT CODE                     
INVTCLTQ EQU   519                 INVALID TRAFFIC CLT SEQ NUMBER               
CHATCLT  EQU   520                 CAN'T CHANGE TRAF CLT/SEQ                    
CHATPRD  EQU   543                 CAN'T CHANGE TRAF PROD CODE                  
INVOFC   EQU   544                 INVALID OFFICE CODE #                        
OFCONE   EQU   1068                ONE CHAR OFFICE CODE REQUIRED                
OFCTWO   EQU   1069                TWO CHAR OFFICE CODE REQUIRED                
INVAAGY  EQU   545                 INVALID ACCOUNT AGENCY CODE                  
SYSNOPEN EQU   1071                ACC SYSTEM NOT OPEN                          
SYSWS    EQU   1072                CAN'T SWITCH TO ACC SYSTEM                   
NOACCCMP EQU   1073                UNABLE TO READ ACC COMPANY REC               
NOBRD    EQU   835                 NO MORE BRD BUYING USE BPOL/TPOL             
SURPHUND EQU   865                 INPUT %-AGE MAY NOT SURPASS 100.00           
MEDRX    EQU   1201                MED R AND/OR X CANT EXIST FOR CLT            
DUPSTATN EQU   245                 DUPLICATE ENTRY                              
INTRSCDT EQU   1305                DATES INTERECT WITH DATES IN DEAL &T         
SAMEREP  EQU   1306                REP IS ALREADY USED IN DEAL &T               
RMVDLLIN EQU   1307                CANNOT REMOVE LINES FROM A DEAL              
DLPRD24M EQU   1308                PERIOD CANNOT SPAN MORE 24 MONTHS            
TOOMNYST EQU   1309                LIMIT OF 75 STATIONS PER DEAL RECORD         
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        TABLE FOR MEDIA AND ELEMENT CODES                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1  BASE=*,LABEL=*                                                   
         OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         OI    GENSTAT4,NODELLST                                                
         OI    GENSTAT3,OKVALSEL                                                
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000ABC'  A(RCPACK) IN CORE                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         ST    R1,VRCPACK          SAVE THIS OFF                                
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
*                                                                               
         LA    R2,MPFKTBL                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,LPFKTBL                                                       
         GOTO1 INITPFKY,DMCB,(R2)                                               
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    SETUP50                                                          
*                                                                               
         CLI   PFKEY,7             PAGE UP?                                     
         BNE   SETUP10                                                          
         CLI   CURRSTPG,0          ARE WE ON THE 1ST PAGE ALREADY?              
         BE    SETUP50                                                          
         MVI   PAGEUPDN,C'-'       PAGE UP                                      
         B     SETUP50                                                          
*                                                                               
SETUP10  CLI   PFKEY,8             PAGE DOWN?                                   
         BNE   SETUP20                                                          
         CLI   DLMLLSTH+7,0        HAVE A STATION ON THE LAST LINE?             
         BE    SETUP50             YES, NOHTING TO DO                           
         MVI   PAGEUPDN,C'+'       PAGE DOWN                                    
         B     SETUP50                                                          
*                                                                               
SETUP20  DS    0H                                                               
*                                                                               
SETUP50  MVI   PFKEY,0             UNKNOWN PFKEY, MAKE IT AN ENTER              
SETUPX   J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
MPFKTBL  DS    0H                                                               
* PAGE UP                                                                       
         DC    AL1(MPF07X-*,07,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF07X   EQU   *                                                                
                                                                                
* PAGE DOWN                                                                     
         DC    AL1(MPF08X-*,08,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF08X   EQU   *                                                                
                                                                                
*        RETURN CALLER                                                          
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
         DC    X'FF'                                                            
********                                                                        
LPFKTBL  DS    0H                                                               
*                                                                               
* DEAL DISPLAY                                                                  
         DC    AL1(LPF02X-*,02,PFTCPROG,(LPF02X-LPF02)/KEYLNQ,0)                
         DC    CL3'S',CL8'DEAL',CL8'DISPLAY'                                    
LPF02    DC    AL1(KEYTYCUR,L'LSTMEDIA-1),AL2(LSTMEDIA-LSTDLNO)                 
         DC    AL1(KEYTYCUR,L'LSTDLNO-1),AL2(LSTDLNO-LSTDLNO)                   
LPF02X   EQU   *                                                                
*                                                                               
* DEAL CHANGE                                                                   
         DC    AL1(LPF03X-*,03,PFTCPROG,(LPF03X-LPF03)/KEYLNQ,0)                
         DC    CL3'C',CL8'DEAL',CL8'CHANGE '                                    
LPF03    DC    AL1(KEYTYCUR,L'LSTMEDIA-1),AL2(LSTMEDIA-LSTDLNO)                 
         DC    AL1(KEYTYCUR,L'LSTDLNO-1),AL2(LSTDLNO-LSTDLNO)                   
LPF03X   EQU   *                                                                
         DC    X'FF'                                                            
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
         EJECT                                                                  
         LTORG                                                        *         
         EJECT                                                                  
       ++INCLUDE SPGENDEAL                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
VRCPACK  DS    A                                                                
*                                                                               
HALF2    DS    H                                                                
SECALPHA DS    CL2                 SECURITY AGENCY ALPHA                        
QXIT     DS    X                   QUICK EXIT FLAG TO SKIP VALREC               
*                                                                               
MISCFLG1 DS    XL1                 MISC FLAGS SET 1                             
MF1KYCHG EQU   X'80'                - KEY FIELD HAS CHANGED                     
*                                                                               
FLAG1    DS    XL1                 AGYFLAG1                                     
*                                                                               
CURRSTN  DS    XL1                 CURRENT STATION # IN RECORD                  
CURRSTPG DS    XL1                 CURRENT PAGE OF STATIONS                     
PAGEUPDN DS    CL1                 - / +  FOR  UP / DOWN RESPECTIVELY           
*                                                                               
SVCOFFC  DS    C                   BACKUP OFFICE NUMBER                         
SVSDLKEY DS    XL(L'SDLKEY)        BACK UP KEY                                  
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
*                                                                               
PERVALST DS    XL(L'PVALOUTB)      PERVAL STORAGE AREA                          
*                                                                               
SCANTAB  DS    CL32                SCANNER INPUT TABLE                          
*                                                                               
KEY1     DS    CL48                                                             
KEY2     DS    CL48                                                             
*                                                                               
SYSSW    DS    XL1                 SE NUM FOR SWITCH                            
POWCODE  DS    CL2                 ACCOUNT AGENCY OVERRIDE                      
ACCOFF   DS    CL2                 ACCOUNT OFFICE CODE                          
OFFLEN   DS    XL1                 ACCOUNT OFFICE LENGTH                        
COMPCD   DS    XL1                 AGENCY BINARY CODE                           
GTFACTB  DS    CL88                                                             
STACOUNT DS    XL1                 # OF STATIONS IN DEAL                        
CTKEY    DS    CL28                                                             
SVDATE   DS    XL3                                                              
CLTLASTC DS    X                                                                
*                                                                               
MYCLTCOD DS    CL3                 TMP CLT CODE(2) & PROD SEQ#(1)               
SVAGMD   DS    XL1                 SAVED AGENCY/MEDIA                           
SVAGMDLO DS    XL1                 SAVED AGENCY/MEDIA LOW  RANGE                
SVAGMDHI DS    XL1                 SAVED AGENCY/MEDIA HIGH RANGE                
SVDNO    DS    XL2                 SAVED DEAL NUMBER                            
SVLREP   DS    XL2                 SAVED LISTING REP                            
OFCBLK   DS    XL(OFCLENQ)                                                      
PRVIDELM DS    XL(SDLIDLNQ)        PREVIOUS ID ELEM                             
PRMIDELM DS    XL(SDLIDLNQ)        PRIMARY ID ELEM                              
SVSTELEM DS    XL(SDLSTLNQ)                                                     
         EJECT                                                                  
***********************************************************************         
         PRINT  ON                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM3DD          MAINTENACE SCREEN                            
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM3CD          LIST SCREEN                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE FAFACTS           FOR FACTSD IN VALACC                         
       ++INCLUDE CTGENFILE         FOR CTSYSD IN VALACC                         
       ++INCLUDE DDOFFICED         FOR OFFICED                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY          AGENCY PROFILES                              
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
PRTLIND  DSECT                                                                  
PRTDLNO  DS    CL5                                                              
         DS    CL1                                                              
PRTDLNM  DS    CL30                                                             
         DS    CL1                                                              
PRTPRIOD DS    CL17                                                             
         DS    CL1                                                              
PRTMEDIA DS    CL3                                                              
         DS    CL1                                                              
PRTREPCD DS    CL3                                                              
         DS    CL1                                                              
PRTSTALS DS    CL69                132 COLUMN REPORT LEAVE SOME ROOM            
PRTLINX  EQU   *                                                                
*                                                                               
LSTLINSD DSECT                                                                  
LSTSELH  DS    CL(L'DLLSEL1H)                                                   
LSTSEL   DS    CL(L'DLLSEL1)                                                    
LSTLINH  DS    CL(L'DLLLLN1H)                                                   
LSTDLNO  DS    CL5                                                              
         DS    CL1                                                              
LSTDLNM  DS    CL30                                                             
         DS    CL2                                                              
LSTMEDIA DS    CL1                                                              
         DS    CL4                                                              
LSTPRIOD DS    CL17                                                             
         DS    CL1                                                              
LSTREPCD DS    CL3                                                              
*                                                                               
STALIND  DSECT                                                                  
STALSTAH DS    XL8                                                              
STALSTA  DS    XL5                                                              
STALGRSH DS    XL8                                                              
STALGRS  DS    XL9                                                              
STALNETH DS    XL8                                                              
STALNET  DS    XL9                                                              
STALPCTH DS    XL8                                                              
STALPCT  DS    XL3                                                              
STALBYRH DS    XL8                                                              
STALBYR  DS    XL30                                                             
STALNXTL EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPSFM70   10/01/07'                                      
         END                                                                    
