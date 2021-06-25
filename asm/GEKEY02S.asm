*          DATA SET GEKEY02S   AT LEVEL 077 AS OF 05/01/02                      
*PHASE TF0502A                                                                  
*INCLUDE MEDGET                                                                 
***********************************************************************         
*                                                                     *         
*  TITLE: TF0502 - PFM INTERFACE OVERLAY FOR SPOT SYSTEM              *         
*                                                                     *         
*  CALLED FROM: PFM INTERFACE CONTROLLER (TF0500), WHICH CALLS        *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  OUTPUTS: KEY FOR THE RECORD TYPE ACCORDING TO USER INPUT           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TF0502 - PFM INTERFACE OVERLAY FOR SPOT SYSTEM'                 
TF0502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TF0502*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   GETMSYS,23          USES GETMSG FOR SYSTEM 23                    
         MVC   RCPROG(2),=C'FM'    PREFIX FOR REPORT NO.                        
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
EXIT02   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SCREEN                                                 *         
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,CONRCRDH         POINT TO RECORD FIELD FIRST                  
         CLI   5(R2),0             NO RECORD?                                   
         BE    MISSFLD             MISSING RECORD                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R4,RECTABLE         POINT TO RECTABLE                            
         CLI   8(R2),C'?'          USER ASKED FOR HELP?                         
         BNE   VKRECLP             NOPE, CHECK ENTRY                            
*                                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         OI    STATFLAG,X'01'      HELP INVOKED                                 
         USING CONHEADH-64,RA                                                   
         XC    CONRCRD,CONRCRD                                                  
         OI    CONRCRDH+6,X'80'    TRANSMIT THE DATA                            
         GOTO1 CLRSCN,DMCB,CONP0H                                               
         GOTO1 DISPHELP,DMCB,RECTABLE                                           
         B     EXIT02                                                           
*                                                                               
VKRECLP  CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    INVLFLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R4)                                                    
         BE    VKPARTS             PARTS OF THE KEY                             
         LA    R4,L'RECTABLE(R4)                                                
         B     VKRECLP                                                          
         EJECT                                                                  
VKPARTS  DS    0H                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         TM    STATFLAG,X'01'      HELP INVOKED?                                
         USING CONHEADH-64,RA                                                   
         BZ    VKPARTS5                                                         
         GOTO1 CLRSCN,DMCB,CONP0H  CLEAR THE SCREEN                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         NI    STATFLAG,X'FF'-X'01'  HELP INVOKED?                              
         USING CONHEADH-64,RA                                                   
VKPARTS5 L     R4,8(R4)            GET ADDRESS OF KEY COMPONENTS                
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    R4,R1                    TO RELOCATION FACTOR                    
         NI    6(R2),X'FF'-X'40'   DON'T PUT CURSOR HERE AGAIN                  
         LA    R2,CONP0H           POINT TO 1ST PROTECTED FIELD                 
         ZIC   R1,0(R2)            GET LENGTH TO NEXT FIELD                     
         AR    R1,R2               POINT TO THE 1ST INPUT FIELD                 
         OI    6(R1),X'40'         CURSOR SHOULD BE HERE                        
         LR    R3,R4               SAVE POSITION IN TABLE                       
VKPARTLP CLI   0(R4),X'FF'         NO MORE PARTS OF THE KEY?                    
         BNE   VKPART10            THERE'S MORE                                 
         LR    R4,R3               DONE, NOW VALIDATE COMPONENTS                
*                                                                               
VKCLR    DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'20'         PROTECT FIELD                                
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         ZIC   R1,0(R2)            NO, BUMP TO INPUT FIELD                      
         AR    R2,R1                                                            
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R1,CONPFH           ARE WE AT THE END YET?                       
         CR    R2,R1                                                            
         BNL   VK00                YES, GO VALIDATE INPUT                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT PROTECT FIELD                   
         B     VKCLR                                                            
*                                                                               
VKPART10 OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         MVC   8(8,R2),0(R4)                                                    
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         NI    1(R2),X'FF'-X'20'   UNPROTECT FIELD                              
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         LA    R4,LTAB(R4)         NEXT COMNPONENT IN KEY                       
         B     VKPARTLP                                                         
         EJECT                                                                  
***********************************************************************         
* CODE TO EXPEDITE THE NEW TABLES FOR RECORD ENTRIES                  *         
***********************************************************************         
VK00     LA    R2,CONI0H                                                        
         XC    INTKEY,INTKEY                                                    
         MVI   CXFLAG,X'FF'        HEX FLAG ON                                  
         MVI   INSERT,X'00'        GET NEXT INSERT POSITION                     
*                                                                               
VK10     DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         LAST FIELD DONE?                             
         BNE   VK40                                                             
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(2),=C'K,'                                                
         CLI   1(R4),X'00'         NO RECORD CODE                               
         BH    VK20                                                             
         MVC   SAVEKEY+2(L'SAVEKEY-2),INTKEY   INSERT HEADER                    
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK20     CLI   1(R4),X'01'         1-BYTE RECORD CODE                           
         BH    VK30                                                             
         GOTO1 HEXOUT,DMCB,2(R4),SAVEKEY+2,1                                    
         MVC   SAVEKEY+4(L'SAVEKEY-4),INTKEY   INSERT HEADER                    
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK30     GOTO1 HEXOUT,DMCB,2(R4),SAVEKEY+2,2   2-BYTE REC. CODE                 
         MVC   SAVEKEY+6(L'SAVEKEY-6),INTKEY   INSERT HEADER                    
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
*                                                                               
VK40     DS    0H                                                               
*                                                                               
         MVC   LENGTH,5(R2)        GET LENGTH OF FIELD                          
         CLC   LENGTH,8(R4)        INPUT LEN LESS THAN MIN LEN?                 
         BL    INVLFLD             YES, INVALID ENTRY                           
         CLC   LENGTH,9(R4)        INPUT LEN GREATER THAN MAX LEN?              
         BH    INVLFLD             YES, INVALID ENTRY                           
         MVC   PADNUM,9(R4)        GET DEFAULT NUMBER OF CHARS.                 
*                                                                               
         L     RF,10(R4)           GET A(BUILD ROUTINE)                         
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    RF,R1                 TO RELOCATION FACTOR                       
         BASR  RE,RF               GET FIELD, INSERT INTO KEY                   
         LA    R4,LTAB(R4)         NEXT KEY COMPONENT                           
         LA    R2,DENTRY(R2)       BUMP TO NEXT SCREEN HEADER                   
         B     VK10                                                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
*                                                                               
VK50     DS    0H                                                               
*                                                                               
         MVC   DISKADDR(L'DISKA+L'INTKEY),DISKA                                 
         USING CONHEADH-64,RA                                                   
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* PREPARE KEY FOR INCOMING HEX FIELD                                  *         
***********************************************************************         
VXKEY    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         STCM  R3,15,SAVER3        SAVE R3                                      
         ZIC   R3,FLDLEN           GET FIELD LENGTH                             
         LA    R4,0(R3,R4)                                                      
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    EXIT02              YES RETURN                                   
         ICM   R3,15,SAVER3        RESTORE R3                                   
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         STCM  R3,15,SAVER3        SAVE R3                                      
         BE    EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* INSERT CHARACTERS TO KEY                                            *         
***********************************************************************         
VCHAR    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'00'        CHAR. STRING SO FAR?                         
         BE    VCHAR10                                                          
         MVI   0(R3),C'('          NO, INSERT CHAR. HEADER                      
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VCHAR10  DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      DEFAULT TO SPACES                            
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         SR    R3,R4               POIN R3 TO FIELD                             
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT02              STOP                                         
*                                                                               
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CHAR. NUMBERS                                        *         
***********************************************************************         
VNUM     NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BNE    VNUM10                                                          
         MVI   0(R3),C'('          YES, INSERT CHAR. HEADER                     
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VNUM10   DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=10C'0'     DEFAULT TO ZEROS                             
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT02              STOP                                         
*                                                                               
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         SR    R3,R4                                                            
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH AGENCY                                               *         
***********************************************************************         
VAGY     NTR1                                                                   
         CLI   LENGTH,0                                                         
         BNE   VAGY10              IF NO ENTRY                                  
         MVC   8(2,R2),14(RA)      GET DEFAULT AGENCY                           
         MVI   5(R2),2                                                          
         MVC   LENGTH,5(R2)                                                     
         OI    6(R2),X'80'                                                      
VAGY10   DS    0H                                                               
         CLI   LENGTH,2            AGENCY MUST BE 2 CHARS.                      
         BNE   INVLFLD                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
         ST    R2,AGYHADD          STORE HEADER ADDRESS                         
         BAS   RE,VCHAR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY ONLY, KEY WILL BE BUILT WITH AGENCY/MEDIA           *         
***********************************************************************         
VAGENCY  NTR1                                                                   
         CLI   LENGTH,0                                                         
         BNE   VAGENCY1            IF NO ENTRY                                  
         MVC   8(2,R2),14(RA)      GET DEFAULT AGENCY                           
         MVI   5(R2),2                                                          
         MVC   LENGTH,5(R2)                                                     
         OI    6(R2),X'80'                                                      
VAGENCY1 DS    0H                                                               
         CLI   LENGTH,2            AGENCY MUST BE 2 CHARS.                      
         BNE   INVLFLD                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
         ST    R2,AGYHADD          STORE HEADER ADDRESS                         
* NOW READ THE SF RECORD                                                        
         L     RE,AIO                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC              GET THE AGENCY RECORD                        
*                                                                               
         L     R3,AIO                                                           
         USING AGYHDRD,R3                                                       
         GOTO1 GETREC                                                           
         MVC   AGYCTRY,AGYPROF+7   EXTRACT COUNTRY CODE                         
         DROP  R3                                                               
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATION AGENCY CODE                                  *         
***********************************************************************         
VMAGY    NTR1                                                                   
         BAS   RE,VCHAR                                                         
         MVI   INTKEY+9,C'0'       PATCH KEY                                    
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH REP AGENCY CODE                                      *         
***********************************************************************         
VRAGY    NTR1                                                                   
         BAS   RE,VCHAR                                                         
         MVI   INTKEY+8,C'0'       PATCH KEY                                    
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH SCHEME AGENCY CODE                                   *         
***********************************************************************         
VSAGY    NTR1                                                                   
         BAS   RE,VAGENCY                                                       
         L     R3,AGYHADD                                                       
         GOTO1 =V(MEDGET),DMCB,(C'T',8(R3)),DATAMGR,WORK,RR=RELO                
         CLI   8(R1),X'FF'                                                      
         BE    INVLFLD                                                          
*                                                                               
         MVI   FLDLEN,2                                                         
         BAS   RE,VXKEY                                                         
         ICM   R3,15,SAVER3                                                     
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(1),WORK     AGENCY & MEDIA                               
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH AGENCY/MEDIA                                         *         
***********************************************************************         
VMEDIA   NTR1                                                                   
         LA    R3,MEDIALST         GET MEDIA LIST                               
         LA    R4,5                NUMBER OF ITEM IN LIST                       
VMEDIA05 CLC   0(1,R3),8(R2)       CHECK IF FIELD IN LIST                       
         BE    VMEDIA10                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,VMEDIA05                                                      
         B     MISSFLD                                                          
VMEDIA10 DS    0H                                                               
         ST    R2,MEDHADD          SAVE MEDIA HEADER ADDRESS                    
         L     R3,AGYHADD                                                       
         GOTO1 =V(MEDGET),DMCB,(8(R2),8(R3)),DATAMGR,WORK,RR=RELO               
         CLI   8(R1),X'FF'                                                      
         BE    INVLFLD                                                          
*                                                                               
         MVI   FLDLEN,2                                                         
         BAS   RE,VXKEY                                                         
         ICM   R3,15,SAVER3                                                     
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(1),WORK     AGENCY & MEDIA                               
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATIOIN MEDIA                                       *         
***********************************************************************         
VMMED    NTR1                                                                   
         STCM  R2,15,MEDHADD                                                    
         MVC   INTKEY(2),=C'(M'    MARKET RECORD HEADER                         
         B     VSMED00                                                          
VRMED    NTR1                                                                   
         STCM  R2,15,MEDHADD                                                    
         MVC   INTKEY(2),=C'(R'    REP RECORD HEADER                            
         B     VSMED00                                                          
VSMED    NTR1                                                                   
         STCM  R2,15,MEDHADD                                                    
         MVC   INTKEY(2),=C'(S'    STATION RECORD HEADER                        
VSMED00  MVC   INTKEY+2(16),=16C'0' PAD WITH CHAR ZEROS                         
         MVI   INTKEY+18,C')'                                                   
         MVI   INSERT,X'02'                                                     
         MVI   CXFLAG,X'00'        CHAR FLAG ON                                 
         BAS   RE,VCHAR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATION CALL LETTER                                  *         
***********************************************************************         
VCALL    NTR1                                                                   
         BAS   RE,VCHAR                                                         
         L     R3,MEDHADD                                                       
         CLI   8(R3),C'T'                                                       
         BNE   EXIT02                                                           
         MVI   INTKEY+7,C'T'                                                    
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATION CLIENT                                       *         
***********************************************************************         
VSCLT    NTR1                                                                   
         BAS   RE,VCHAR                                                         
         MVI   INTKEY+13,C'0'      PATCH KEY                                    
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CLIENT                                               *         
***********************************************************************         
VCLT     NTR1                                                                   
         CLC   =C'ALL',8(R2)                                                    
         BE    VCLT10                                                           
         OC    8(3,R2),SPACES      IF 2 CHARS., PAD WITH SPACE                  
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB+4,C'R'         READ                                         
         MVC   DMCB+5(3),=X'000A14' CLPACK CORE-RES PHASE NUMBER                
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB             A(CLIENT PACK ROUTINE)                       
         GOTO1 (RF),(R1),8(R2),WORK                                             
         MVC   SAVEKEY+1(2),WORK                                                
         CLI   0(R1),0                                                          
         BNE   INVLFLD                                                          
         ST    R2,CLTHADD          STORE CLIENT HEADER ADDRESS                  
*                                                                               
         MVI   FLDLEN,4                                                         
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),2                                         
         B     EXIT02                                                           
*                                                                               
VCLT10   MVI   FLDLEN,4                                                         
         XC    8(4,R2),8(R2)       NULL DATA                                    
         MVI   5(R2),0             RESET LENGTH                                 
         MVI   LENGTH,0                                                         
         ST    R2,CLTHADD          STORE CLIENT HEADER ADDRESS                  
         XC    WORK,WORK           NULL WORK SPACE                              
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),2 STICK NULLS IN CLIENT                   
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH PRODUCT                                              *         
***********************************************************************         
VPRD     NTR1                                                                   
         CLI   LENGTH,0                                                         
         BNE   VPRD10                                                           
         MVC   8(3,R2),=C'POL'                                                  
         B     VPRD40                                                           
VPRD10   DS    0H                                                               
         OC    8(3,R2),SPACES      IF 2 CHARS., PAD WITH SPACE                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SAVEKEY                                                 
         GOTO1 HIGH                READ THE CLIENT HDR RECORD                   
         CLC   KEY(13),KEYSAVE     EXACT MATCH ONLY                             
         BE    VPRD20                                                           
         L     R2,CLTHADD          POINT TO CLIENT NOT PRODUCT                  
         B     INVLFLD                                                          
VPRD20   L     RE,AIO                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC              GET THE RECORD FROM ITS DA                   
         L     R3,AIO                                                           
         USING CLTHDRD,R3                                                       
         LA    R3,CLIST                                                         
         DROP  R3                                                               
VPRD30   CLI   0(R3),0             END OF TABLE?                                
         BE    INVLFLD             NO MATCH ON PRODUCT                          
         CLC   8(3,R2),0(R3)                                                    
         BE    VPRD40                                                           
         LA    R3,4(R3)                                                         
         B     VPRD30                                                           
VPRD40   DS    0H                                                               
         MVI   FLDLEN,2                                                         
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R4,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,3(R3),0(R4),1  COPY THE CODE                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH PRODUCT                                              *         
***********************************************************************         
VPROD    NTR1                                                                   
         OC    8(3,R2),SPACES      IF 2 CHARS., PAD WITH SPACE                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),SAVEKEY                                                 
         GOTO1 HIGH                READ THE CLIENT HDR RECORD                   
         CLC   KEY(13),KEYSAVE     EXACT MATCH ONLY                             
         BE    VPROD20                                                          
         L     R2,CLTHADD          POINT TO CLIENT NOT PRODUCT                  
         B     INVLFLD                                                          
VPROD20  L     RE,AIO                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC              GET THE RECORD FROM ITS DA                   
         L     R3,AIO                                                           
         USING CLTHDRD,R3                                                       
         LA    R3,CLIST                                                         
         DROP  R3                                                               
VPROD30  CLI   0(R3),0             END OF TABLE?                                
         BE    INVLFLD             NO MATCH ON PRODUCT                          
         CLC   8(3,R2),0(R3)                                                    
         BE    VPROD40                                                          
         LA    R3,4(R3)                                                         
         B     VPROD30                                                          
VPROD40  DS    0H                                                               
         LA    R4,INTKEY           POINT TO INTKEY                              
         ZIC   R5,INSERT           POINT TO INSERT POSITION                     
         LA    R4,0(R4,R5)         COMPUTE INSERT POSITION                      
         LA    R5,3(R5)                                                         
         STC   R5,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'00'        CHAR STRING SO FAR?                          
         BE    VPROD50                                                          
         MVI   0(R4),C'('                                                       
         LA    R4,1(R4)            BUMP FORWARD                                 
         ZIC   R5,INSERT                                                        
         LA    R5,1(R5)                                                         
         STC   R5,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VPROD50  DS    0H                                                               
         MVC   0(3,R4),8(R2)                                                    
         MVI   3(R4),C')'                                                       
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SPOT LENGTH (GOAL)                                         *         
***********************************************************************         
VSPLEN   NTR1                                                                   
         ST    R2,SPLHADD          SAVE SPOT LENGTH HEADER ADDRESS              
         BAS   RE,VBIN                                                          
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BINARY (TOTAL SEC - GOAL)                            *         
***********************************************************************         
VTOTSEC  NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VTOTSEC5            THEN COPY SPOT LENGTH                        
         BAS   RE,VBIN                                                          
         B     EXIT02                                                           
VTOTSEC5 L     R4,SPLHADD          COPY SPOT LENGTH                             
         LR    R2,R4                                                            
         MVC   LENGTH,5(R2)        UPDATE LENGTH!                               
         BAS   RE,VBIN                                                          
         B     EXIT02                                                           
***********************************************************************         
* BUILD KEY WITH BINARY                                               *         
***********************************************************************         
VBIN     NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VBIN30                                                           
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         CLI   PADNUM,4                                                         
         BL    VBIN05                                                           
         C     R4,=F'65535'        BOUNDARY CHECK FOR 2-BYTE BINARY             
         BH    INVLFLD                                                          
         STCM  R4,3,WORK                                                        
         B     VBIN30                                                           
VBIN05   CLI   PADNUM,3                                                         
         BL    VBIN10                                                           
         CH    R4,=H'255'          BOUNDARY CHECK FOR 1-BYTE BINARY             
         BH    INVLFLD                                                          
         STC   R4,WORK                                                          
         B     VBIN20                                                           
VBIN10   CH    R4,=H'99'           BOUNDARY CHECK FOR YEAR                      
         BH    INVLFLD                                                          
VBIN20   STC   R4,WORK                                                          
*                                                                               
VBIN30   LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   PADNUM,4                                                         
         BL    VBIN40                                                           
         LA    R4,2(R4)            2-BYTE BINARY                                
VBIN40   LA    R4,2(R4)            1-BYTE BINARY                                
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BE    VBIN50                                                           
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
*                                                                               
VBIN50   DS    0H                                                               
         CLI   PADNUM,3                                                         
         BH    VBIN60                                                           
         MVC   0(2,R3),=C'00'                                                   
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
VBIN60   DS    0H                                                               
         MVC   0(4,R3),=C'0000'                                                 
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),2                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* GET MARKET NUMBER                                                   *         
***********************************************************************         
VMARKET  NTR1                                                                   
         ST    R2,MKTHADD          SAVE MARKET HEADER ADDRESS                   
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         TM    4(R2),X'08'         THIS INPUT NUMERIC?                          
         BZ    INVLFLD                                                          
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION FIELD ONLY                                         *         
***********************************************************************         
VSTATION NTR1                                                                   
         TM    4(R2),X'04'         THIS INPUT ALPHABETIC?                       
         BZ    INVLFLD                                                          
         OC    8(4,R2),SPACES      PAD WITH SPACE                               
         MVI   5(R2),4                                                          
         ST    R2,STTHADD          SAVE STATION HEADER ADDRESS                  
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH MARKET/STATION                                       *         
***********************************************************************         
VSTAT    NTR1                                                                   
         L     R3,MKTHADD          IF NO MARKET                                 
         CLI   5(R3),0                                                          
         BH    VSTAT05                                                          
         CLI   LENGTH,0            AND NO STATION                               
         BE    EXIT02              THEN STOP                                    
         L     R2,MKTHADD                                                       
         B     INVLFLD             STATION W/O MARKET=ERROR                     
VSTAT05  CLI   LENGTH,0            ERROR IF MARKET IS ENTERED W/O               
         BE    INVLFLD             STATION                                      
         TM    4(R2),X'04'         MUST BE ALPHABETIC                           
         BZ    INVLFLD                                                          
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         LA    R4,10(R4)            1-BYTE BINARY                               
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BE    VSTAT10                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VSTAT10  DS    0H                                                               
         MVC   WORK1(4),=C'0000'   PAD FIELD WITH ZEROS                         
         L     R4,MKTHADD                                                       
         ZIC   R5,5(R4)            GET LENGTH OF MARKET                         
         BCTR  R5,0                SUBTRACT 1 FOR EX                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),8(R4)      GET MARKET                                   
         LA    R4,WORK1                                                         
         LA    R4,3(R4)            RIGHT JUSTIFY                                
         SR    R4,R5                                                            
         EX    R5,*+8              R5 ALREADY SUBTRACTED BY 1                   
         B     *+10                                                             
         MVC   0(0,R4),WORK2       STORE MARKET                                 
*                                                                               
VSTAT30  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,AGYCTRY                                                 
*                                                                               
         L     R2,MEDHADD          GET MEDIA HEADER ADDRESS                     
         MVC   STAPMED,8(R2)       MOVE MEDIA CODE                              
*                                                                               
         MVC   STAPQMKT,WORK1      MOVE MARKET NUMBER                           
         MVC   STAPQSTA,8(R2)      MOVE STATION                                 
         MVC   STAPACOM,ACOMFACS                                                
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A7A' STAPACK                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             GET STAPACK ADDRESS                          
         GOTO1 (RF),(R4)                                                        
         CLI   STAPERR,0                                                        
         BNE   INVLFLD                                                          
*                                                                               
         GOTO1 HEXOUT,DMCB,STAPMKST,0(R3),5                                     
         B     EXIT02                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH MONTH                                                *         
***********************************************************************         
VMONTH   NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VMONTH30                                                         
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BNZ   VMONTH20                                                         
*                                                                               
         LA    R3,MONTHLST         CHECK IF MONTH LIST                          
         LA    R4,12               12 MONTHS IN A YEAR                          
VMONTH10 CLC   0(3,R3),8(R2)                                                    
         BE    VMONTH15                                                         
         LA    R3,3(R3)            BUMP TO NEXT MONTH                           
         BCT   R4,VMONTH10                                                      
         B     INVLFLD             MONTH NOT FOUND                              
VMONTH15 LA    R3,13               CALCUTE NUMERIC EQUIVALENT                   
         SR    R3,R4                                                            
         STC   R3,WORK                                                          
         B     VMONTH30                                                         
VMONTH20 ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         CH    R4,=H'12'           BOUNDARY CHECK FOR MONTH                     
         BH    INVLFLD                                                          
         CH    R4,=H'1'            BOUNDARY CHECK FOR MONTH                     
         BL    INVLFLD                                                          
         STC   R4,WORK                                                          
*                                                                               
VMONTH30 DS    0H                                                               
         MVI   FLDLEN,2                                                         
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         MVC   0(2,R3),=C'01'                                                   
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BAND                                                 *         
***********************************************************************         
VBAND    NTR1                                                                   
         L     R3,MEDHADD                                                       
         CLI   8(R3),C'R'          RADIO?                                       
         BE    VBAND10                                                          
         MVC   8(1,R2),8(R3)       INSERT THE BUY MEDIA HERE                    
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'         RETRANSMIT                                   
         B     VBAND20                                                          
*                                                                               
VBAND10  DS    0H                                                               
         CLI   8(R2),C'A'          AM?                                          
         BE    VBAND20                                                          
         CLI   8(R2),C'F'          FM?                                          
         BNE   INVLFLD                                                          
*                                                                               
VBAND20  DS    0H                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY       NEW STATION 15-BYTE KEY                      
         MVI   KEY,C'S'                                                         
         L     R3,MEDHADD                                                       
         MVC   KEY+1(1),8(R3)      MEDIA LETTER                                 
         L     R3,STTHADD                                                       
         MVC   KEY+2(4),8(R3)      STATION CALL LETTERS                         
         MVC   KEY+6(1),8(R2)      BAND IF ANY                                  
         L     R3,AGYHADD                                                       
         MVC   KEY+7(2),8(R3)      AGENCY LETTERS                               
         L     R3,CLTHADD                                                       
         MVC   KEY+9(3),8(R3)      CLIENT LETTERS                               
         BAS   RE,STAHI                                                         
         L     R3,AIO                                                           
         CLC   KEY(15),0(R3)                                                    
         BE    VBAND30                                                          
         MVC   KEY+9(3),=3C'0'     CLIENT LETTERS                               
         BAS   RE,STAHI            READ THE STATION RECORD                      
         L     R3,AIO                                                           
         CLC   KEY(15),0(R3)                                                    
         BNE   INVLFLD                                                          
*                                                                               
         USING STATHDR,R3                                                       
VBAND30  MVC   DUB(L'SMKT),SMKT                                                 
         DROP  R3                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A1B' MSPACK                                    
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB             A(MARKET/STATION PACK ROUTINE)               
         GOTO1 (RF),(R1),DUB,KEY+2,WORK                                         
*                                                                               
         MVI   FLDLEN,10                                                        
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),5                                         
         B     EXIT02                                                           
*                                                                               
STAHI    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH LINE                                                 *         
***********************************************************************         
VLINE    NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VLINE10                                                          
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         CH    R4,=H'255'          BOUNDARY CHECK FOR 1-BYTE BINARY             
         BH    INVLFLD                                                          
         STC   R4,WORK                                                          
*                                                                               
VLINE10  LA    R3,INTKEY+22        GET INTKEY                                   
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BE    VLINE20                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         MVI   CXFLAG,X'FF'                                                     
*                                                                               
VLINE20  DS    0H                                                               
         MVC   0(2,R3),=C'00'                                                   
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH SPILL                                                *         
***********************************************************************         
VSPILL   NTR1                                                                   
         LA    R3,INTKEY+20        POINT TO INTKEY+10                           
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VSPILL10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
VSPILL10 MVC   0(2,R3),=C'00'                                                   
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
*                                                                               
         CLI   8(R2),C'Y'                                                       
         BNE   VSPILL20                                                         
         MVC   0(2,R3),=C'80'                                                   
         B     EXIT02                                                           
VSPILL20 CLI   8(R2),C'N'                                                       
         BNE   INVLFLD                                                          
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BUYER                                                *         
***********************************************************************         
VBUYER   NTR1                                                                   
         OC    8(3,R2),SPACES      PAD WITH SPACE                               
         MVI   5(R2),3                                                          
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=X'0D65'     VERIFY BUYER                                 
         MVC   KEY+2(1),SAVEKEY    A/M                                          
         MVC   KEY+3(3),8(R2)                                                   
         GOTO1 HIGH                READ IN THE BUYER REC                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
         L     R1,AIO                                                           
         ST    R1,AREC                                                          
         GOTO1 GETREC              READ THE RECORD IN                           
         L     R1,AIO                                                           
         USING BYRRECD,R1                                                       
         MVC   WORK(1),BYRCODE                                                  
         MVC   SAVEKEY+1(1),BYRCODE                                             
         DROP  R1                                                               
*                                                                               
         MVI   FLDLEN,2                                                         
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
VBUYER10 GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CAMPAIGN                                             *         
***********************************************************************         
VCAMP    NTR1                                                                   
         SR    R3,R3               USED TO FIGURE OUT 1'S COMPLEMENT            
         CLI   LENGTH,0            NOTHING INPUTTED?                            
         BE    VCAMP10                                                          
         TM    4(R2),8             NUMERIC?                                     
         BZ    INVLFLD             NO                                           
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         C     R3,=F'65535'        > X'FFFF'?                                   
         BH    INVLFLD             YES                                          
*                                                                               
VCAMP10  X     R3,=F'65535'        1'S COMPLEMENT FOR HELP WORD                 
         STCM  R3,3,WORK                                                        
         STCM  R3,3,SAVEKEY+2                                                   
*                                                                               
         MVI   FLDLEN,4                                                         
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),2                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH NWH MARKET                                           *         
***********************************************************************         
VDMKT    NTR1                                                                   
         CLI   LENGTH,0            NOTHING INPUTTED?                            
         BE    VDMKT10                                                          
         TM    4(R2),8             NUMERIC?                                     
         BZ    INVLFLD             NO                                           
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         STCM  R3,3,SAVEKEY+4                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D67'     VERIFY BUYER                                 
         MVC   KEY+2(6),SAVEKEY    A/M                                          
         GOTO1 HIGH                READ IN THE BUYER REC                        
         CLC   KEY(8),KEYSAVE                                                   
         BNE   INVLFLD                                                          
         MVC   WORK(2),KEY+8       COPY CAMP/MKT SEQ #                          
*                                                                               
VDMKT10  LA    R3,INTKEY+4         POINT TO INTKEY+4                            
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VDMKT20                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         MVI   CXFLAG,X'FF'                                                     
VDMKT20  DS    0H                                                               
         CLI   LENGTH,0                                                         
         BNE   VDMKT30                                                          
         MVC   0(4,R3),=C'0000'                                                 
         B     EXIT02                                                           
VDMKT30  GOTO1 HEXOUT,DMCB,WORK,0(R3),2                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATION/MARKET                                       *         
***********************************************************************         
VSSTA    NTR1                                                                   
         CLI   LENGTH,0            NO INPUT?                                    
         BNE   VSSTA03             YES, MOVE NULLS IN                           
         XC    WORK,WORK                                                        
         MVI   FLDLEN,10                                                        
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),5                                         
         B     EXIT02                                                           
*                                                                               
VSSTA03  TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    VSSTA05             NO, INPUT IS STATION                         
         ZIC   R4,LENGTH           YES, STICK MKT TO HIGH ORDER 2 BYTES         
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)      DECIMAL YEAR                                 
         CVB   R4,DUB                                                           
         C     R4,=F'65535'        MKT > X'FFFF'?                               
         BH    INVLFLD                                                          
         XC    WORK,WORK                                                        
         STCM  R4,3,WORK                                                        
         MVI   FLDLEN,10                                                        
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),5                                         
         B     EXIT02                                                           
*                                                                               
VSSTA05  DS    0H                                                               
         CLI   LENGTH,4            INPUT IS STATION                             
         BH    INVLFLD                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'T'          MEDIA LETTER ALWAYS T                        
         OC    8(4,R2),SPACES      PAD WITH SPACE                               
         MVC   KEY+2(4),8(R2)      STATION CALL LETTERS                         
         MVI   KEY+6,C'T'          NO BAND                                      
         L     R3,AGYHADD                                                       
         MVC   KEY+7(2),8(R3)      AGENCY LETTERS                               
         L     R3,CLTHADD                                                       
         MVC   KEY+9(3),8(R3)      (SCHEME) CLIENT LETTERS                      
         BAS   RE,SSTAHI                                                        
         L     R3,AIO                                                           
         CLC   KEY(17),0(R3)                                                    
         BE    VSSTA10                                                          
         MVC   KEY+9(3),=3C'0'     (SCHEME) CLIENT LETTERS                      
         BAS   RE,SSTAHI            READ THE STATION RECORD                     
         L     R3,AIO                                                           
         CLC   KEY(17),0(R3)                                                    
         BNE   INVLFLD                                                          
*                                                                               
         USING STATHDR,R3                                                       
VSSTA10  MVC   DUB(L'SMKT),SMKT                                                 
         DROP  R3                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A1B' MSPACK                                    
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB             A(MARKET/STATION PACK ROUTINE)               
         GOTO1 (RF),(R1),DUB,KEY+2,WORK                                         
*                                                                               
         MVI   FLDLEN,10                                                        
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),5                                         
         B     EXIT02                                                           
*                                                                               
SSTAHI   NTR1                      DONT READ                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH DAY PART COED                                        *         
***********************************************************************         
VDAYPT   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VDAYPT10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VDAYPT10 DS    0H                                                               
         ZIC   R4,INSERT                                                        
         LA    R4,2(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVC   0(2,R3),=C'00'      MOVE IN NULLS                                
         CLI   LENGTH,0            IF NO INPUT EXIT                             
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,8(R2),0(R3),1                                        
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BOOK YEAR (PERIOD)                                   *         
***********************************************************************         
VPBKYR   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VPBKYR10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VPBKYR10 DS    0H                                                               
         ZIC   R4,INSERT                                                        
         LA    R4,14(R4)                                                        
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVC   0(14,R3),=14C'0'                                                 
         BAS   RE,VBKYR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BOOK YEAR (NSID)                                     *         
***********************************************************************         
VNBKYR   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VNBKYR10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VNBKYR10 DS    0H                                                               
         ZIC   R4,INSERT                                                        
         LA    R4,2(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVC   0(2,R3),=C'00'      DTP# ALWAYS NULL                             
         BAS   RE,VBKYR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BOOK YEAR                                            *         
***********************************************************************         
VBKYR    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VBKYR10                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VBKYR10  DS    0H                                                               
         ZIC   R4,INSERT                                                        
         LA    R4,2(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   LENGTH,0            ANY INPUT?                                   
         BNE   VBKYR20                                                          
         MVC   0(2,R3),=C'00'      NOPE, DEFAULT TO NULLS                       
         B     EXIT02                                                           
VBKYR20  TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH           GET LENGTH OF INPUT                          
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)      DECIMAL YEAR                                 
         CVB   R4,DUB                                                           
         LCR   R5,R4               GET 2'S COMPLEMENT                           
         BCTR  R5,0                CHANGE TO 1'S COMPLEMENT                     
         STC   R5,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BOOK MONTH                                           *         
***********************************************************************         
VBKMON   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VBKMON10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,3(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VBKMON10 DS    0H                                                               
         CLI   LENGTH,0            ANY INPUT?                                   
         BNE   VBKMON20                                                         
         MVC   0(2,R3),=C'00'      NOPE, DEFAULT TO NULLS                       
         B     EXIT02                                                           
VBKMON20 TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH           GET LENGTH OF INPUT                          
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)      DECIMAL YEAR                                 
         CVB   R4,DUB                                                           
         CH    R4,=H'12'           SOURCE VALID?                                
         BH    INVLFLD                                                          
         CH    R4,=H'1'            INPUT BETWEEN 1 AND 12?                      
         BL    INVLFLD                                                          
         STC   R4,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         MVI   0(R3),C'8'                                                       
         B     EXIT02                                                           
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MYERROR  GOTO1 ERREX2                                                           
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
         EJECT                                                                  
*                                                                               
MEDIALST DC    C'TRNXC'            MEDIA LIST                                   
MONTHLST DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
*                                CL8 - NAME OF THE RECORD TYPE                  
*                                AL4 - ADDRESS OF KEY PORTIONS                  
*                                                                               
RECTABLE DS    0CL12                                                            
         DC    C'AGY     ',AL4(AGYREC)                                          
         DC    C'CLT     ',AL4(CLTREC)                                          
         DC    C'PRD     ',AL4(PRDREC)                                          
         DC    C'EST     ',AL4(ESTREC)                                          
         DC    C'GOAL    ',AL4(GOALREC)                                         
         DC    C'STABILL ',AL4(STABREC)                                         
         DC    C'BILL    ',AL4(BILLREC)                                         
         DC    C'BUY     ',AL4(BUYREC)                                          
         DC    C'NWH     ',AL4(NWHREC)                                          
         DC    C'NWD     ',AL4(NWDREC)                                          
         DC    C'STATION ',AL4(STATREC)                                         
         DC    C'MKT     ',AL4(MKTREC)                                          
         DC    C'REP     ',AL4(REPREC)                                          
         DC    C'SCHEME  ',AL4(SCHREC)                                          
         DC    C'PERIOD  ',AL4(PERREC)                                          
         DC    C'NSID    ',AL4(NSIDREC)                                         
         DC    C'DETAIL  ',AL4(DETREC)                                          
         DC    C'PW-MKT  ',AL4(PWMKTREC)                                        
         DC    C'PW-STA  ',AL4(PWSTAREC)                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                CL8 - NAME OF THE FIELD TO INPUT               
*                                XL1 - MINIMUM LENGTH OF INPUT                  
*                                XL1 - MAXIMUM LENGTH OF INPUT                  
*                                AL4 - ADDRESS OF VALIDATING ROUTINE            
*                                                                               
*                                                                               
*                FIELD     MIN   MAX   VAL                                      
*                NAME      LEN   LEN   ADD                                      
*              ------------------------------------                             
AGYREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGY)                                
         DC    X'FF',X'0106'                                                    
CLTREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
STA      DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
STA1     DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    X'FF',X'0100'                                                    
PRDREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'00',X'03',AL4(VPROD)                               
         DC    X'FF',X'0100'                                                    
ESTREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPROD)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'0100'                                                    
GOALREC  DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPRD)                                
         DC    C'MARKET #',X'00',X'05',AL4(VBIN)                                
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'DAYPART ',X'00',X'01',AL4(VDAYPT)                              
         DC    C'SPOT LEN',X'00',X'03',AL4(VSPLEN)                              
         DC    C'TOT. SEC',X'00',X'03',AL4(VTOTSEC)                             
         DC    C'AGY CODE',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'0102'                                                    
STABREC  DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPRD)                                
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'MARKET #',X'00',X'04',AL4(VMARKET)                             
         DC    C'STATION ',X'00',X'05',AL4(VSTAT)                               
         DC    X'FF',X'020E01'                                                  
BILLREC  DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPROD)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'YEAR SVC',X'00',X'02',AL4(VBIN)                                
         DC    C'MON. SVC',X'00',X'09',AL4(VMONTH)                              
         DC    C'BILL MON',X'00',X'09',AL4(VMONTH)                              
         DC    C'BILL #  ',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'0100'                                                    
BUYREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'00',X'03',AL4(VPRD)                                
         DC    C'STATION ',X'03',X'04',AL4(VSTATION)                            
         DC    C'BAND    ',X'00',X'01',AL4(VBAND)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'LINE    ',X'00',X'03',AL4(VLINE)                               
         DC    C'SPILL   ',X'00',X'01',AL4(VSPILL)                              
         DC    X'FF',X'00'                                                      
*                                                                               
NWHREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'BUYER   ',X'02',X'03',AL4(VBUYER)                              
         DC    C'CAMPAIGN',X'00',X'05',AL4(VCAMP)                               
         DC    C'MARKET  ',X'00',X'04',AL4(VBIN)                                
         DC    X'FF',X'020D67'                                                  
*                                                                               
NWDREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'BUYER   ',X'02',X'03',AL4(VBUYER)                              
         DC    C'CAMPAIGN',X'00',X'05',AL4(VCAMP)                               
         DC    C'MARKET  ',X'00',X'04',AL4(VDMKT)                               
         DC    X'FF',X'020D68'                                                  
*                                                                               
STATREC  DC    C'MEDIA   ',X'01',X'01',AL4(VSMED)                               
         DC    C'CALL LET',X'00',X'05',AL4(VCALL)                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VCHAR)                               
         DC    C'CLIENT  ',X'00',X'03',AL4(VSCLT)                               
         DC    X'FF',X'00'                                                      
*                                                                               
MKTREC   DC    C'MEDIA   ',X'01',X'01',AL4(VMMED)                               
         DC    C'MKT CODE',X'00',X'04',AL4(VNUM)                                
         DC    C'AGY CODE',X'00',X'02',AL4(VMAGY)                               
         DC    X'FF',X'00'                                                      
*                                                                               
REPREC   DC    C'MEDIA   ',X'01',X'01',AL4(VRMED)                               
         DC    C'REP CODE',X'00',X'03',AL4(VNUM)                                
         DC    C'AGY CODE',X'00',X'02',AL4(VRAGY)                               
         DC    X'FF',X'00'                                                      
*                                                                               
SCHREC   DC    C'AGENCY  ',X'00',X'02',AL4(VSAGY)                               
         DC    C'SCHEME  ',X'00',X'03',AL4(VCLT)                                
         DC    X'FF',X'010C'                                                    
*                                                                               
PERREC   DC    C'AGENCY  ',X'00',X'02',AL4(VSAGY)                               
         DC    C'SCHEME  ',X'00',X'03',AL4(VCLT)                                
         DC    C'YEAR    ',X'00',X'02',AL4(VPBKYR)                              
         DC    X'FF',X'010C'                                                    
*                                                                               
NSIDREC  DC    C'AGENCY  ',X'00',X'02',AL4(VSAGY)                               
         DC    C'SCHEME  ',X'00',X'03',AL4(VCLT)                                
         DC    C'STATION ',X'00',X'05',AL4(VSSTA)                               
         DC    C'DAYPART ',X'00',X'01',AL4(VDAYPT)                              
         DC    C'YEAR    ',X'00',X'02',AL4(VNBKYR)                              
         DC    C'PERIOD  ',X'00',X'02',AL4(VBKMON)                              
         DC    X'FF',X'010C'                                                    
*                                                                               
DETREC   DC    C'AGENCY  ',X'00',X'02',AL4(VSAGY)                               
         DC    C'SCHEME  ',X'00',X'03',AL4(VCLT)                                
         DC    C'STATION ',X'00',X'05',AL4(VSSTA)                               
         DC    C'DAYPART ',X'00',X'01',AL4(VDAYPT)                              
         DC    C'SEQ NUM ',X'00',X'03',AL4(VBIN)                                
         DC    C'YEAR    ',X'00',X'02',AL4(VBKYR)                               
         DC    C'PERIOD  ',X'00',X'02',AL4(VBKMON)                              
         DC    X'FF',X'010C'                                                    
*                                                                               
PWMKTREC DS    0X                                                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPRD)                                
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'MARKET #',X'00',X'04',AL4(VBIN)                                
         DC    X'FF',X'020D7A'                                                  
*                                                                               
PWSTAREC DS    0X                                                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPRD)                                
         DC    C'ESTIMATE',X'01',X'03',AL4(VBIN)                                
         DC    C'STATION ',X'03',X'04',AL4(VSTATION)                            
         DC    C'BAND    ',X'00',X'01',AL4(VBAND)                               
         DC    X'FF',X'020D7A'                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
DCODE    EQU   CONP1H-CONP0H                                                    
DENTRY   EQU   CONI1H-CONI0H                                                    
LTAB     EQU   STA1-STA                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         PRINT ON                                                               
       ++INCLUDE GEKEYFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
       ++INCLUDE GEPFMSAVE         (OUR MAINTENANCE SCREEN OVERLAY)             
         PRINT OFF                                                              
       ++INCLUDE SPSTAPACKD                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
STATHDR  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPNWSBYR          BUYER RECORD                                 
         EJECT                                                                  
       ++INCLUDE GEKEYWORKD        (SYSTEM AREAS)                               
*        PRINT ON                                                               
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
DISKA    DS    XL4                                                              
INTKEY   DS    CL60                                                             
SAVEKEY  DS    CL(L'INTKEY)                                                     
PREVKEY  DS    CL(L'INTKEY)                                                     
AREC     DS    A                                                                
AGYHADD  DS    A                   AGENCY FIELD HEADER ADDRESS                  
MEDHADD  DS    A                   MEDIA FIELD HEADER ADDRESS                   
CLTHADD  DS    A                   CLIENT FIELD HEADER ADDRESS                  
MKTHADD  DS    A                   MARKET FIELD HEADER ADDRESS                  
STTHADD  DS    A                   STATION FIELD HEADER ADDRESS                 
SPLHADD  DS    A                   SPOT LENGTH HEADER ADDRESS                   
SAVER3   DS    F                   SAVES CONTENT OF REGISTER 3                  
LENGTH   DS    X                   INPUT FIELD LENGTH                           
PADNUM   DS    X                   NUMBER OF PADDING CHARS.                     
CXFLAG   DS    X                   X'00' IF CHAR, X'FF' IF HEX                  
INSERT   DS    X                   NEXT INSERT POSITION                         
FLDLEN   DS    X                   OUTPUT FIELD LENGTH                          
AGYCTRY  DS    C                   COUNTRY CODE                                 
WORK1    DS    CL13                                                             
WORK2    DS    CL13                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077GEKEY02S  05/01/02'                                      
         END                                                                    
