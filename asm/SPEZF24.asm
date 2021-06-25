*          DATA SET SPEZF24    AT LEVEL 017 AS OF 06/07/11                      
*PHASE T23024A                                                                  
         TITLE 'T23001 - BATCH MOVE RECORD'                                     
***********************************************************************         
*                                                                     *         
*  TITLE: T23001 - EASI BATCH MOVE RECORDS                            *         
*  COMMENTS: THIS PROGRAM DOES LIST AND DISPLAY FOR BATCH MOVE RECS   *         
*            WHICH ARE STORED ON GENDIR/GENFIL                        *         
*  OUTPUTS: UPDATED BATCH MOVE RECORDS                                *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 -                                                       *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG                                                        
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - NOT USED                                              *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
***********************************************************************         
T23024   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3024**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         MVC   AIO,AIO1                                                         
         ST    R2,RELO                                                          
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, NO DDS TEST                      
         BE    *+12                                                             
         CLI   1(RA),C'*'          ONLY DDS TERMINALS                           
         BNE   DDSONLY                                                          
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,RECADD         ADD RECORD ILLEGAL                           
         BE    INVAL                                                            
         CLI   MODE,RECDEL         DELETE RECORD ILLEGAL                        
         BE    INVAL                                                            
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    INVAL                                                            
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VKEY     DS    0H                                                               
         XC    FLTRS(FLTRSLQ),FLTRS                                             
         NI    MISCFLG1,X'FF'-MF1KYCHG   ASSUME NO KEY FIELDS CHANGED           
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALIMED             VALIMED SETS TO SPOT OR NET                  
*                                                                               
         LA    R2,LINSTAH          STATION                                      
         BRAS  RE,CHKVAL                                                        
         CLI   5(R2),0                                                          
         BE    VK30                                                             
*                                                                               
         GOTO1 VALISTA                                                          
         MVC   FSTA,QSTA                                                        
*                                                                               
VK30     DS    0H                                                               
         OI    4(R2),X'20'         INDICATE STA VALIDATED                       
*                                                                               
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVC   SYSDIR(6),=C'GENDIR'                                             
         MVC   SYSFIL(6),=C'GENFIL'                                             
         MVI   CURSYST,C'C'        CONTROL                                      
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         LA    R2,LINBATH          BATCH DATE                                   
         BRAS  RE,CHKVAL                                                        
         CLI   5(R2),0                                                          
         BE    VK60                                                             
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,WORK+6)                                  
         XC    WORK+6(2),=X'FFFF'                                               
         MVC   FSBDATE,WORK+6      SAVE DATE FILTER                             
         MVC   FEBDATE,WORK+6      SAVE DATE FILTER                             
*                                                                               
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK60                YES                                          
*                                                                               
         LA    R3,1+8(R2,R3)                                                    
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,WORK+6)                                  
         XC    WORK+6(2),=X'FFFF'                                               
         MVC   FEBDATE,WORK+6      SAVE DATE FILTER                             
*                                                                               
VK60     DS    0H                                                               
         OI    4(R2),X'20'         INDICATE DATE VALIDATED                      
*                                                                               
         LA    R2,LINOPTH                                                       
         BRAS  RE,CHKVAL                                                        
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         BRAS  RE,VFTR                                                          
*                                                                               
VK100    DS    0H                                                               
         OI    4(R2),X'20'         INDICATE FILTERS VALIDATED                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EZMKEY,R4                                                        
         MVC   EZMKID,=C'ZM'       RECORD ID                                    
*                                                                               
VKXIT    B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DKEY     DS    0H                                                               
         LA    R4,KEY                                                           
         USING EZMKEY,R4                                                        
*                                                                               
         MVC   LINSTA,SPACES                                                    
         GOTO1 APRTSTA,DMCB,EZMKSTA,LINSTA                                      
         OI    LINSTAH+6,X'80'                                                  
*                                                                               
         LA    R2,LINBATH          BATCH DATE                                   
         MVC   RQDTE,EZMKDTP                                                    
         XC    RQDTE,=X'FFFF'                                                   
         GOTO1 DATCON,DMCB,(2,RQDTE),(5,8(R2))                                  
*                                                                               
DKXIT    B     XIT                                                              
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
* DISPLAY RECORD ROUTINE *                                                      
*                                                                               
DREC     DS    0H                                                               
         LA    R6,KEY                                                           
         USING EZMKEY,R6                                                        
*                                                                               
         LA    R2,LININVH          INVOICE #                                    
         MVC   8(L'LININV,R2),EZMKINV                                           
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DRXIT                                                            
*                                                                               
         USING EZMDTAEL,R6                                                      
*                                                                               
         LA    R2,LINOLDUH                                                      
         XC    8(L'LINOLDU,R2),8(R2)                                            
         MVC   8(L'LINOLDU,R2),EZMFAGY                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,LINNEWUH         NEW ID                                       
         XC    8(L'LINNEWU,R2),8(R2)                                            
         MVC   8(L'LINNEWU,R2),EZMTAGY                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R4,R6               SAVE R6                                      
*                                                                               
         MVC   LINPERS,SPACES                                                   
         OC    EZMPID,EZMPID                                                    
         BZ    DR20                                                             
*                                                                               
         CLC   EZMPID,=X'0FFFF'                                                 
         BH    *+14                                                             
         MVC   LINPERS(3),=C'DDS'                                               
         B     DR10                                                             
*                                                                               
* GET AGENCY CODE FOR THE "FROM" USER ID                                        
         XC    LKAGYBLK,LKAGYBLK                                                
         MVC   LKAGYUID(L'EZMFAGY),EZMFAGY                                      
         BRAS  RE,LKAGY2                                                        
         BNE   DR20                                                             
         DROP  R6                                                               
*                                                                               
* READ ACCESS RECORD FOR THIS AGENCY TO GET SECURITY AGENCY                     
*                                                                               
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         USING CT5REC,R6                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,LKAGYAGY                                                
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY2,(R6),DMWORK              
         CLI   8(R1),0                                                          
         BNE   DR20                                                             
         CLC   KEY2(25),0(R6)                                                   
         BNE   DR20                                                             
*                                                                               
         LA    R6,CT5DATA                                                       
         DROP  R6                                                               
*                                                                               
DR05     DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    DR06                SEC AGY NOT FOUND                            
*                                                                               
         CLI   0(R6),CTSEAELQ      SECURITY AGENCY ELEMENT                      
         BNE   *+14                                                             
         MVC   LKAGYAGY,CTSEAAID-CTSEAD(R6)                                     
         B     DR06                                                             
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR05                                                             
*                                                                               
* READ PASSWORD RECORD                                                          
DR06     DS    0H                                                               
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         USING SA0REC,R6                                                        
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,LKAGYAGY                                                 
         MVC   SA0KNUM,EZMPID-EZMDTAEL(R4) PID                                  
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY2,(R6),DMWORK              
         CLI   8(R1),0                                                          
         BNE   DR20                                                             
         CLC   KEY2(25),0(R6)                                                   
         BNE   DR20                                                             
*                                                                               
         LA    R6,SA0DATA                                                       
         DROP  R6                                                               
*                                                                               
DR07     DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    DR20                                                             
*                                                                               
         CLI   0(R6),SAPALELQ      PERSON ID ELEMENT                            
         BNE   *+14                                                             
         MVC   SVSECPID,SAPALPID-SAPALD(R6)                                     
         B     DR08                                                             
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR07                                                             
*                                                                               
* READ PERSON RECORD                                                            
DR08     DS    0H                                                               
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         USING SAPEREC,R6                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,LKAGYAGY                                                 
         MVC   SAPEPID,SVSECPID                                                 
*                                                                               
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY2,(R6),DMWORK              
         CLI   8(R1),0                                                          
         BNE   DR20                                                             
         CLC   KEY2(23),0(R6)      UP TO THE EFFECTIVE DATE                     
         BNE   DR20                                                             
*                                                                               
         LA    R6,SAPEDATA                                                      
         DROP  R6                                                               
*                                                                               
DR09     DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    DR20                                                             
*                                                                               
         CLI   0(R6),SANAMELQ      NAME ELEMENT                                 
         BNE   DR09C                                                            
*                                                                               
         USING SANAMD,R6                                                        
         LLC   RE,SANAMLN          LENGTH OF ELEMENT                            
         AR    RE,R6               END OF ELEMENT                               
*                                                                               
         LA    R1,LINPERS          FIELD ON SCREEN                              
         LA    R6,SANAMES          LIST OF NAMES                                
         DROP  R6                                                               
*                                                                               
DR09A    DS    0H                                                               
         USING SANAMES,R6          NAME SUB-ELEMENT                             
*                                                                               
         LLC   RF,SANAMELN         NAME LENGTH                                  
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      COPY TO SCREEN                               
*                                                                               
         LLC   RF,SANAMELN         NAME LENGTH                                  
         AR    R1,RF               ADVANCE PAST NAME ON SCREEN                  
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)            LEAVE A SPACE                                
         AR    R6,RF               ADVANCE TO NEXT NAME                         
         AHI   R6,1                                                             
         CR    RE,R6               PAST END OF ELEMENT?                         
         BH    DR09A               NO - DISPLAY NEXT NAME                       
         DROP  R6                                                               
*                                                                               
         B     DR10                ALL DONE                                     
*                                                                               
DR09C    DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     DR09                                                             
*                                                                               
DR10     DS    0H                                                               
         NI    LINPTTLH+1,X'FF'-X'0C'  TURN OFF ZERO VIZ BIT                    
         OI    LINPTTLH+6,X'80'                                                 
         NI    LINPERSH+1,X'FF'-X'0C'  TURN OFF ZERO VIZ BIT                    
         OI    LINPERSH+6,X'80'                                                 
*                                                                               
DR20     DS    0H                                                               
         LR    R6,R4               RESTORE R6                                   
         USING EZMDTAEL,R6                                                      
*                                                                               
         LA    R2,LINMVDTH         MOVE DATE                                    
         XC    8(L'LINMVDT,R2),8(R2)                                            
         GOTO1 DATCON,DMCB,(2,EZMDATE),(5,8(R2))                                
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,LINMVTMH         MOVE TIME                                    
         XC    8(L'LINMVTM,R2),8(R2)                                            
         GOTO1 HEXOUT,DMCB,EZMTIME,WORK,L'LINMVTM                               
         MVC   8(2,R2),WORK                                                     
         MVI   10(R2),C':'                                                      
         MVC   11(2,R2),WORK+2                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,LINSRCEH         SOURCE                                       
         XC    8(L'LINSRCE,R2),8(R2)                                            
         MVC   8(L'LINSRCE,R2),EZMSRCE                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         MVC   LINOSTA,SPACES                                                   
         GOTO1 APRTSTA,DMCB,EZMOLDST,LINOSTA                                    
         CLC   EZMOLDST,SPACES                                                  
         BH    *+10                                                             
         MVC   LINOSTA,LINSTA                                                   
         OI    LINOSTAH+6,X'80'                                                 
*                                                                               
         LA    R4,KEY                                                           
         USING EZMKEY,R4                                                        
         MVC   LINNSTA,SPACES                                                   
         GOTO1 APRTSTA,DMCB,KEY+EZMKSTA-EZMKEY,LINNSTA                          
         OI    LINNSTAH+6,X'80'                                                 
*                                                                               
         LA    R2,LINAGYNH         AGENCY NAME                                  
         XC    8(L'LINAGYN,R2),8(R2)                                            
         MVC   8(L'LINAGYN,R2),EZMAGYNM                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,LINADVNH         ADV NAME                                     
         XC    8(L'LINADVN,R2),8(R2)                                            
         MVC   8(L'LINADVN,R2),EZMADVNM                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,LINPRDNH         PRODUCT NAME                                 
         XC    8(L'LINPRDN,R2),8(R2)                                            
         MVC   8(L'LINPRDN,R2),EZMPRDNM                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,LINSPTSH         SPOTS                                        
         XC    8(L'LINSPTS,R2),8(R2)                                            
         OC    EZMNSPTS,EZMNSPTS                                                
         BZ    DR30                                                             
         EDIT  (C5,EZMNSPTS),(L'LINSPTS,8(R2)),COMMAS=YES,ALIGN=LEFT,  X        
               ZERO=BLANK                                                       
DR30     OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,LINNETH                                                       
         XC    8(L'LINNET,R2),8(R2)                                             
         OC    EZMNDOL,EZMNDOL                                                  
         BZ    DR40                                                             
         EDIT  (C11,EZMNDOL),(L'LINNET,8(R2)),2,COMMAS=YES,ALIGN=LEFT, X        
               ZERO=BLANK                                                       
DR40     OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,LINGRSH                                                       
         XC    8(L'LINGRS,R2),8(R2)                                             
         OC    EZMGDOL,EZMGDOL                                                  
         BZ    DR50                                                             
         EDIT  (C11,EZMGDOL),(L'LINGRS,8(R2)),2,COMMAS=YES,ALIGN=LEFT, X        
               ZERO=BLANK                                                       
DR50     OI    6(R2),X'80'                                                      
*                                                                               
DRXIT    B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LIST     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LS50                                                             
*                                                                               
         LA    R2,LNNTTLE                                                       
         USING LSTA,R2                                                          
*                                                                               
         MVC   LSTA,=CL7'STATION'                                               
         NC    LSTA,UNSPACES                                                    
         MVC   LBDAT,=CL8'BT. DATE'                                             
         NC    LBDAT(2),UNSPACES                                                
         NC    LBDAT+4(4),UNSPACES                                              
         MVC   LINV,=CL10'INVOICE'                                              
         NC    LINV,UNSPACES                                                    
         MVC   LOID,=CL8'OLD ID'                                                
         NC    LOID(3),UNSPACES                                                 
         MVC   LNID,=CL8'NEW ID'                                                
         NC    LNID(3),UNSPACES                                                 
         MVC   LMOS,=CL6'MOS'                                                   
         MVC   LMDAT,=CL8'MV. DATE'                                             
         NC    LMDAT(2),UNSPACES                                                
         NC    LMDAT+4(4),UNSPACES                                              
         MVC   LMTIM,=CL5'TIME'                                                 
         NC    LMTIM,UNSPACES                                                   
*                                                                               
         DROP  R2                                                               
         OI    LNNTTLEH+6,X'80'                                                 
         B     LS100                                                            
*                                                                               
LS50     DS    0H                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,=A(HDRTN)                                                     
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
LS100    DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EZMKEY,R4                                                        
*                                                                               
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BNZ   *+14                YES, RE-DISPLAY THE RECORDS                  
         MVC   KEY(L'EZMKEY),LASTKEY                                            
         B     LS120                                                            
*                                                                               
         OC    FEBDATE,FEBDATE     DATES ARE INVERTED, START WITH END           
         BZ    *+10                                                             
         MVC   EZMKDTP,FEBDATE                                                  
*                                                                               
         OC    FOUSER,FOUSER                                                    
         BZ    *+10                                                             
         MVC   EZMOUID,FOUSER                                                   
*                                                                               
         OC    FNUSER,FNUSER                                                    
         BZ    *+10                                                             
         MVC   EZMNUID,FNUSER                                                   
*                                                                               
         OC    FSTA,FSTA                                                        
         BZ    *+10                                                             
         MVC   EZMKSTA,FSTA                                                     
*                                                                               
         OC    FINV,FINV                                                        
         BZ    *+10                                                             
         MVC   EZMKINV,FINV                                                     
*                                                                               
LS120    DS    0H                                                               
         MVC   EZMKID,=C'ZM'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     LS220                                                            
*                                                                               
LS200    GOTO1 SEQ                                                              
*                                                                               
LS220    DS    0H                                                               
         CLC   EZMKID,KEYSAVE                                                   
         BE    *+14                                                             
         XC    LASTKEY,LASTKEY                                                  
         B     LS700                                                            
*                                                                               
         OC    FSBDATE,FSBDATE      WAS SPECIFIC BATCH DATE REQUESTED           
         BZ    LS240                NO                                          
*                                                                               
         CLC   EZMKDTP,FEBDATE                                                  
         BL    LS200                                                            
         CLC   EZMKDTP,FSBDATE                                                  
         BH    LS200                                                            
*                                                                               
LS240    DS    0H                                                               
         OC    FOUSER,FOUSER                                                    
         BZ    LS250                                                            
*                                                                               
         CLC   EZMOUID,FOUSER                                                   
         BNE   LS200                                                            
*                                                                               
LS250    DS    0H                                                               
         OC    FNUSER,FNUSER                                                    
         BZ    LS260                                                            
*                                                                               
         CLC   EZMNUID,FNUSER                                                   
         BNE   LS200                                                            
*                                                                               
LS260    DS    0H                                                               
         OC    FSTA,FSTA           WAS SPECIFIC STATION REQUESTED               
         BZ    LS280                NO                                          
*                                                                               
         MVC   WORK,EZMKSTA                                                     
         OC    WORK,SPACES                                                      
         CLC   FSTA,WORK                                                        
         BNE   LS200                                                            
*                                                                               
LS280    DS    0H                                                               
         OC    FINV,FINV                                                        
         BZ    LS300                                                            
*                                                                               
         MVC   WORK,EZMKINV                                                     
         OC    WORK,SPACES                                                      
         CLC   FINV,WORK                                                        
         BNE   LS200                                                            
*                                                                               
LS300    DS    0H                                                               
         MVC   LASTKEY,KEY                                                      
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+14                                                             
         MVC   LSTA(30),=30C'-'                                                 
         B     LS200                                                            
*                                                                               
         USING EZMDTAEL,R6                                                      
*                                                                               
* CHECK FILTERS HERE                                                            
*                                                                               
         OC    FMDATE,FMDATE                                                    
         BZ    *+14                                                             
         CLC   EZMDATE,FMDATE                                                   
         BNE   LS200                                                            
*                                                                               
         OC    FMOS,FMOS                                                        
         BZ    *+14                                                             
         CLC   EZMMOS,FMOS                                                      
         BNE   LS200                                                            
*                                                                               
         OC    FSRCE,FSRCE                                                      
         BZ    *+14                                                             
         CLC   EZMSRCE,FSRCE                                                    
         BNE   LS200                                                            
*                                                                               
* NOW DISPLAY THE LIST LINE                                                     
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
* STATION CALL LETTERS                                                          
         MVC   LSTA,SPACES                                                      
         GOTO1 APRTSTA,DMCB,EZMKSTA,LSTA                                        
*                                                                               
* BATCH DATE                                                                    
         MVC   WORK(2),EZMKDTP                                                  
         XC    WORK(2),=X'FFFF'                                                 
         GOTO1 DATCON,DMCB,(2,WORK),(5,LBDAT)                                   
*                                                                               
* INVOICE NUMBER                                                                
         MVC   LINV,EZMKINV                                                     
*                                                                               
* OLD USER ID                                                                   
         MVC   LOID,EZMFAGY        OLD AGY ID                                   
*                                                                               
* NEW USER ID                                                                   
         MVC   LNID,EZMTAGY        NEW AGY ID                                   
*                                                                               
* MOS                                                                           
         MVC   WORK(4),EZMMOS                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,WORK+6)                                  
         MVC   LMOS,WORK+6                                                      
*                                                                               
* MOVE DATE                                                                     
         GOTO1 DATCON,DMCB,(2,EZMDATE),(5,LMDAT)                                
*                                                                               
* MOVE TIME                                                                     
         GOTO1 HEXOUT,DMCB,EZMTIME,WORK,L'LMTIM                                 
         MVC   LMTIM(2),WORK                                                    
         MVI   LMTIM+2,C':'                                                     
         MVC   LMTIM+3(2),WORK+2                                                
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LS600                                                            
*                                                                               
         MVC   DMDSKADD,KEY+36                                                  
         GOTO1 LISTMON                                                          
         B     LS200                                                            
*                                                                               
LS600    MVC   PSTA,LSTA                                                        
         MVC   PBDAT,LBDAT                                                      
         MVC   PINV,EZMKINV                                                     
         MVC   POID,EZMFAGY        OLD AGY ID                                   
         MVC   PNID,EZMTAGY        NEW AGY ID                                   
         MVC   PMOS,EZMMOS                                                      
         MVC   PMDAT,LMDAT                                                      
         MVC   PMTIM,LMTIM                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    LISTAR,LISTAR                                                    
         B     LS200                                                            
*                                                                               
LS700    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
*                                                                               
CLCINV   CLC   FTRINVNO(0),EZMKINV    SAME INVIOCE                              
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
INVAL    MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
INVALER  MVI   ERROR,INVALID                                                    
         LA    R2,CONRECH                                                       
         B     TRAPERR                                                          
DDSONLY  MVI   ERROR,INVREC                                                     
         LA    R2,CONRECH                                                       
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
BADATE   MVI   ERROR,INVDATE                                                    
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
*                                                                               
*                                                                               
***********************************************************************         
* VALIDATE FILTERS (IF ANY)                                           *         
***********************************************************************         
VFTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRQX              NO                                           
*                                                                               
         CLI   8(R2),C'?'          HELP                                         
         BE    VFERHELP            YES                                          
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    VFERHELP            YES                                          
*                                                                               
         LA    RE,BLOCK            ADDRESS OF FIRST BLOCK                       
         LHI   RF,L'BLOCK                                                       
         XCEFL                                                                  
*                                                                               
         LA    R4,BLOCK+1                                                       
         GOTO1 SCANNER,DMCB,(R2),(5,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    VFEROPT                                                          
         MVC   BLOCK(1),4(R1)      SAVE NUMBER OF LINES                         
*                                                                               
VFTR10   CLI   BLOCK,0                                                          
         BNH   VFTRQX                                                           
*                                                                               
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'MDATE'                                               
         BE    VFTR120                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'MOS'                                                 
         BE    VFTR140                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'INVOICE'                                             
         BE    VFTR160                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'OUSER'                                               
         BE    VFTR180                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'NUSER'                                               
         BE    VFTR190                                                          
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'SOURCE'                                              
         BE    VFTR200                                                          
*                                                                               
         MVC   BYTE,4(R4)                                                       
         B     VFEROPT                                                          
*                                                                               
* MDATE                                                                         
*                                                                               
VFTR120  DS    0H                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, NO DDS TEST                      
         BNE   VFEROFFL                                                         
*                                                                               
         LA    R5,22(R4)                                                        
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         OC    DMCB,DMCB                                                        
         BNZ   *+14                                                             
         MVC   BYTE,8(R4)                                                       
         B     VFERDAT                                                          
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,WORK+6)                                  
         MVC   FMDATE,WORK+6       SAVE MOVE DATE FILTER                        
*                                                                               
         B     VFTR900             NEXT OPTION                                  
*                                                                               
* MOS                                                                           
*                                                                               
VFTR140  DS    0H                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, NO DDS TEST                      
         BNE   VFEROFFL                                                         
*                                                                               
         LA    R5,22(R4)                                                        
         GOTO1 DATVAL,DMCB,(2,(R5)),WORK                                        
         OC    DMCB,DMCB                                                        
         BNZ   *+14                                                             
         MVC   BYTE,8(R4)                                                       
         B     VFERDAT                                                          
*                                                                               
         MVC   FMOS,WORK           SAVE MOS FILTER                              
*                                                                               
         B     VFTR900             NEXT OPTION                                  
*                                                                               
* INVOICE                                                                       
*                                                                               
VFTR160  DS    0H                                                               
         MVC   BYTE,8(R4)                                                       
         CLI   1(R4),10                                                         
         BH    VFEROPT                                                          
         MVC   FINV,22(R4)         INVOICE                                      
         OC    FINV,SPACES                                                      
*                                                                               
         B     VFTR900             NEXT OPTION                                  
*                                                                               
* USER ID (OLD USER ID)                                                         
*                                                                               
VFTR180  DS    0H                                                               
         MVC   BYTE,8(R4)                                                       
         CLI   1(R4),8                                                          
         BH    VFEROPT                                                          
*                                                                               
         XC    LKAGYBLK,LKAGYBLK                                                
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LKAGYUID(0),22(R4)                                               
         OC    LKAGYUID,SPACES                                                  
         BRAS  RE,LKAGY2                                                        
         BNE   VFEROPT                                                          
         MVC   FOUSER,LKAGYBID                                                  
*                                                                               
         B     VFTR900             NEXT OPTION                                  
*                                                                               
* NEW USER ID                                                                   
*                                                                               
VFTR190  DS    0H                                                               
         MVC   BYTE,8(R4)                                                       
         CLI   1(R4),8                                                          
         BH    VFEROPT                                                          
*                                                                               
         XC    LKAGYBLK,LKAGYBLK                                                
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LKAGYUID(0),22(R4)                                               
         OC    LKAGYUID,SPACES                                                  
         BRAS  RE,LKAGY2                                                        
         BNE   VFEROPT                                                          
         MVC   FNUSER,LKAGYBID                                                  
*                                                                               
         B     VFTR900             NEXT OPTION                                  
*                                                                               
* SOURCE                                                                        
*                                                                               
VFTR200  DS    0H                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE, NO DDS TEST                      
         BNE   VFEROFFL                                                         
*                                                                               
         MVC   BYTE,8(R4)                                                       
         CLI   1(R4),4                                                          
         BH    VFEROPT                                                          
         MVC   FSRCE,22(R4)                                                     
*                                                                               
         B     VFTR900             NEXT OPTION                                  
*                                                                               
VFTR900  DS    0H                                                               
         LA    R4,32(,R4)                                                       
         ZIC   RF,BLOCK            DECREMENT NUM. OF LINES                      
         BCTR  RF,0                                                             
         STC   RF,BLOCK                                                         
         B     VFTR10                                                           
*                                                                               
VFTRNQX  J     NEQXIT                                                           
*                                                                               
VFTRQX   J     EQXIT                                                            
*                                                                               
*                                                                               
VFERUID  L     R1,=A(INVUIDMS)                                                  
         B     VFTRERR                                                          
VFERHELP L     R1,=A(VFTRHELP)                                                  
         B     VFTRERR                                                          
VFEROPT  L     R1,=A(INVOPTMS)                                                  
         B     VFTRERR                                                          
VFERDAT  L     R1,=A(INVDATMS)                                                  
         B     VFTRERR                                                          
VFEROFFL L     R1,=A(OFFLNMS)                                                   
         B     VFTRERR                                                          
*                                                                               
VFTRERR  XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
*                                                                               
         CLI   0(R1),L'CONHEAD-1                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
* CURSOR POSITION WITHIN OPTIONS FIELD - SET IN "BYTE"                          
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,0(R1)                                                         
         USING TIOBD,R1                                                         
         MVC   TIOBCURI,BYTE                                                    
         DROP  R1                                                               
*                                                                               
         GOTO1 ERREX2                                                           
*                                                                               
         DC    AL1(L'INVUIDMS)                                                  
INVUIDMS DC    C'* ERROR * INVALID USER ID'                                     
*                                                                               
         DC    AL1(L'INVOPTMS)                                                  
INVOPTMS DC    C'* ERROR * INVALID OPTION'                                      
*                                                                               
         DC    AL1(L'INVDATMS)                                                  
INVDATMS DC    C'* ERROR * INVALID DATE FORMAT'                                 
*                                                                               
         DC    AL1(L'OFFLNMS)                                                   
OFFLNMS  DC    C'* ERROR * THIS OPTION OFFLINE ONLY'                            
*                                                                               
         DC    AL1(L'VFTRHELP)                                                  
VFTRHELP DC    C'FILTERS=MDATE/MOS/INVOICE/OUSER/NUSER/SOURCE'                  
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,48,C'EASI INVOICE MOVES'                                      
         SSPEC H2,48,C'------------------'                                      
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         SSPEC H8,3,C'STATION'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,12,C'BATCH DATE'                                              
         SSPEC H9,12,C'----------'                                              
         SSPEC H8,24,C'INVOICE NO'                                              
         SSPEC H9,24,C'----------'                                              
         SSPEC H8,36,C'OLD AGY ID'                                              
         SSPEC H9,36,C'----------'                                              
         SSPEC H8,48,C'NEW AGY ID'                                              
         SSPEC H9,48,C'----------'                                              
         SSPEC H8,60,C' MOS'                                                    
         SSPEC H9,60,C'------'                                                  
         SSPEC H8,68,C'MOVE DATE'                                               
         SSPEC H9,68,C'---------'                                               
         SSPEC H8,80,C'MOVE TIME'                                               
         SSPEC H9,80,C'---------'                                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
*                                                                               
* FORMAT STATION FOR PRINTING (EG WABC-FM) *                                    
* P1 -> STATION FIELD (EG WABCF)                                                
* P2 -> 7-CHARACTER OUTPUT FIELD                                                
*                                                                               
PRTSTA   NTR1  BASE=*,LABEL=*                                                   
         L     R2,4(R1)            OUTPUT                                       
         L     R1,0(R1)            INPUT                                        
*                                                                               
         MVC   0(4,R2),0(R1)       OUTPUT CALL LETTERS                          
         OC    0(4,R2),SPACES                                                   
*                                                                               
         LA    R5,3(R2)            4TH CHARACTER                                
         CLI   0(R5),C' '                                                       
         BH    *+6                                                              
         BCTR  R5,0                                                             
         MVI   1(R5),C'-'                                                       
         MVC   2(1,R5),4(R1)       MEDIA                                        
*                                                                               
         CLI   2(R5),C'T'                                                       
         BNE   *+12                                                             
         MVI   3(R5),C'V'          TV                                           
         B     PRTXIT                                                           
*                                                                               
         CLI   2(R5),C'A'                                                       
         BE    *+12                                                             
         CLI   2(R5),C'F'                                                       
         BNE   *+12                                                             
         MVI   3(R5),C'M'          AM OR FM                                     
         B     PRTXIT                                                           
*                                                                               
         CLI   2(R5),C'C'                                                       
         BE    *+12                                                             
         CLI   2(R5),C'S'                                                       
         BNE   *+8                                                              
         MVI   2(R5),C'N'                                                       
*                                                                               
PRTXIT   J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* AGENCY INFORMATION LOOKUP SUBROUTINE                                          
* TAKES IN 10-CHAR USER ID (IN LKAGYBLK,LKAGYUID)                               
* RETURNS 2-BYTE BINARY ID ADN 2-CHAR AGENCY POWER CODE                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
LKAGY2   NTR1  BASE=*,LABEL=*                                                   
         MVI   LKAGYAGY,X'FF'      DEFAULT TO NO LOOKUP                         
         MVC   LKAGYBID,=X'FFFF'                                                
*                                                                               
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         USING CTIKEY,R6                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,LKAGYUID                                                  
         OC    CTIKID,SPACES                                                    
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY2,(R6),DMWORK              
         CLI   8(R1),0                                                          
         BNE   LK2XIT                                                           
         CLC   KEY2(25),0(R6)                                                   
         BNE   LK2XIT                                                           
         LA    R6,CTIDATA                                                       
*                                                                               
LK250    DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    LK2XIT                                                           
*                                                                               
         CLI   0(R6),X'02'         DESCRIPTION ELEM                             
         BNE   *+10                                                             
         MVC   LKAGYBID,2(R6)                                                   
*                                                                               
         CLI   0(R6),X'06'         AGY ID ELEM                                  
         BNE   *+10                                                             
         MVC   LKAGYAGY,2(R6)                                                   
*                                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     LK250                                                            
*                                                                               
LK2XIT   DS    0H                                                               
         CLI   LKAGYAGY,X'FF'                                                   
         JNE   EQXIT                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
         MVI   UNSPACES,X'BF'      LOWERCASE                                    
         MVC   UNSPACES+1(L'UNSPACES-1),UNSPACES                                
         MVI   UNSPACES,X'FF'      FIRST CHAR REMAINS UPPERCASE                 
         L     RF,=A(PRTSTA)                                                    
         A     RF,RELO                                                          
         ST    RF,APRTSTA                                                       
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
CHKVAL   TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZR  RE                  YES - EXIT                                   
         OI    MISCFLG1,MF1KYCHG   INDICATE KEY FIELD HAS BEEN CHANGED          
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                                                                               
*                                                                               
HDRTN    NTR1  BASE=*,LABEL=*                                                   
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFA4D                                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPEZFB4D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPEZFA4D                                                       
*                                                                               
       ++INCLUDE DDGENTWA                                                       
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
* SPEZMOV                                                                       
       ++INCLUDE SPEZMOV                                                        
* SPGENEZ                                                                       
       ++INCLUDE SPGENEZ                                                        
*SPEZFWORKD                                                                     
       ++INCLUDE SPEZFWORKD                                                     
       ++INCLUDE SPEZFSYSD                                                      
*                                                                               
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FATIOB                                                         
*                                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   WRKFREC                                                          
ATIOB    DS    F                                                                
APRTSTA  DS    A                                                                
*                                                                               
MISCFLG1 DS    XL1                                                              
MF1KYCHG EQU   X'80'               A KEY FIELD WAS CHANGED                      
*                                                                               
FLTRS    DS    0H                                                               
* THESE FIELDS ARE IN RECORD KEY                                                
FSBDATE  DS    XL2                 BATCH DATE, INVERTED                         
FEBDATE  DS    XL2                                                              
FOUSER   DS    XL2                                                              
FNUSER   DS    XL2                                                              
FSTA     DS    CL5                                                              
FINV     DS    CL10                                                             
* THESE FIELDS ARE IN RECORD, AVAILABLE ONLY OFFLINE                            
FMDATE   DS    XL2                                                              
FMOS     DS    CL4                                                              
FSRCE    DS    CL4                                                              
FLTRSLQ  EQU   *-FLTRS                                                          
*                                                                               
* AGENCY LOOKUP  BLOCK                                                          
LKAGYBLK DS    0CL14                                                            
LKAGYBID DS    CL2                 BINARY USER ID                               
LKAGYAGY DS    CL2                 AGY ALPHA                                    
LKAGYUID DS    CL10                ALPHA USER ID                                
*                                                                               
UNSPACES DS    CL132                                                            
LASTKEY  DS    XL32                                                             
KEY2     DS    XL32                                                             
KEY2SAVE DS    XL32                                                             
SVSECPID DS    CL8                                                              
*                                                                               
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTA     DS    CL7                                                              
         DS    C                                                                
LBDAT    DS    CL8                                                              
         DS    C                                                                
LINV     DS    CL10                                                             
         DS    C                                                                
LOID     DS    CL8                                                              
         DS    C                                                                
LNID     DS    CL8                                                              
         DS    C                                                                
LMOS     DS    CL6                                                              
         DS    C                                                                
LMDAT    DS    CL8                                                              
         DS    C                                                                
LMTIM    DS    CL5                                                              
*                                                                               
*                                                                               
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PBDAT    DS    CL8                                                              
         DS    CL4                                                              
PINV     DS    CL10                                                             
         DS    CL2                                                              
POID     DS    CL8                                                              
         DS    CL4                                                              
PNID     DS    CL8                                                              
         DS    CL4                                                              
PMOS     DS    CL4                                                              
         DS    CL4                                                              
PMDAT    DS    CL8                                                              
         DS    CL4                                                              
PMTIM    DS    CL5                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPEZF24   06/07/11'                                      
         END                                                                    
