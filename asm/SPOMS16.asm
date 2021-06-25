*          DATA SET SPOMS16    AT LEVEL 005 AS OF 01/16/07                      
*PHASE T23416A                                                                  
T23416   TITLE 'SPOMS16 - LAST METHOD MAINT'                                    
T23416   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 BOXDL+WIDEDL,*T23416*,R7,RR=R3,CLEAR=YES                         
         LR    R0,RC               OUR BOX AREA (TO GET 198 CHAR WIDE)          
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
*                                                                               
         ST    R0,ABOX             OUR BOX AREA (TO GET 198 CHAR WIDE)          
         ST    R0,TWAVBOX                                                       
         LR    R1,R0                                                            
         USING BOXD,R1                                                          
         AH    R0,=Y(BOXDL)                                                     
         ST    R0,BOXAWIDE                                                      
         DROP  R1                                                               
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SVSECAGY,FATAGYSC                                                
         DROP  R3                                                               
*                                                                               
         NI    GENSTAT1,X'FF'-RDUPAPPL  <<===  BECAUSE OF SPOMS0A               
         OI    GLSTSTAT,APPLCDSP+RETEXTRA                                       
         CLI   ACTEQU,ACTLIST                                                   
         BNE   INIT10                                                           
         BAS   RE,SETMFKYS                                                      
         B     INIT20                                                           
INIT10   BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
*                                                                               
INIT20   CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST THE RECORD?                             
         BE    LST                                                              
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVI   MISCFLG1,0                                                       
         MVI   MISCFLG2,0                                                       
***************                                                                 
* VALIDATE THE MEDIA                                                            
***************                                                                 
VKMED00  LA    R2,LSMMEDH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY CHANGED                                  
*                                                                               
         CLI   5(R2),0             NEED THE MEDIA                               
         BE    NEEDFLDS                                                         
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
***************                                                                 
* VALIDATE BUYER                                                                
***************                                                                 
VKBUYR00 LA    R2,LSMBYRH          BUYER                                        
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY CHANGED                                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIBUYR,DMCB,8(R2)  YES, VALIDATE IT                            
         BNE   INVLFLD                                                          
         MVC   SVBUYER,LSMBYR                                                   
         OC    SVBUYER,SPACES                                                   
         OI    4(R2),X'20'         VALIDATED                                    
***************                                                                 
* VALIDATE THE STATION                                                          
***************                                                                 
VKSTA00  DS    0H                                                               
         LA    R2,LSMSTAH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY CHANGED                                  
*                                                                               
         XC    SVSTA,SVSTA                                                      
         CLI   5(R2),0                                                          
         BNE   VKSTA10                                                          
         CLI   ACTEQU,ACTLIST                                                   
         BE    VKSTAX                                                           
         B     MISSFLD                                                          
*                                                                               
VKSTA10  GOTO1 VALISTA                                                          
         OI    MISCFLG2,MF2STA                                                  
         MVC   SVSTA,BSTA                                                       
*                                                                               
VKSTAX   OI    4(R2),X'20'                                                      
***************                                                                 
* VALIDATE THE CLIENT                                                           
***************                                                                 
VKCLT00  DS    0H                                                               
         LA    R2,LSMCLTH                                                       
         TM    4(R2),X'20'                                                      
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY CHANGED                                  
*                                                                               
         XC    SVCLT,SVCLT                                                      
         CLI   5(R2),0             NO CLIENT                                    
         BE    VKCLTX              THAT'S OKAY                                  
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   8(3,R2),QCLT        IN CASE ITS AAN                              
         OI    6(R2),X'80'                                                      
*                                                                               
         OI    MISCFLG2,MF2CLT                                                  
         MVC   SVCLT,BCLT                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'sDAR'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
         CLI   CLTOFFCE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFCE                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,PROFDAR,DATAMGR                                
*                                                                               
VKCLTX   OI    4(R2),X'20'                                                      
         EJECT                                                                  
*****                                                                           
* BUILD THE KEY                                                                 
*****                                                                           
VKBKEY   XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING DMTHKEY,R4          OVERLAY KEY WITH OUR TEMPLATE                
         MVI   DMTHTYPE,DMTHTYPQ                                                
         MVI   DMTHSBTY,DMTHSBTQ                                                
         MVC   DMTHAGMD,BAGYMD                                                  
         MVC   DMTHBYR,SVBUYER                                                  
         MVC   DMTHSTA,BSTA                                                     
         CLI   LSMCLTH+5,0         ANY CLIENT?                                  
         BE    *+10                ALL CLIENT THEN                              
         MVC   DMTHCLT,BCLT                                                     
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         L     R4,AIO                                                           
         USING DAREMTHD,R4                                                      
         MVI   LSMMED,C'T'                                                      
         MVC   LSMBYR,DMTHBYR                                                   
         XC    WORK(5),WORK                                                     
         MVC   WORK+2(3),DMTHSTA                                                
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,LSMSTA                                   
         OC    DMTHCLT,DMTHCLT     ZERO?                                        
         BZ    DKXIT                                                            
         GOTO1 CLUNPK,DMCB,DMTHCLT,LSMCLT                                       
DKXIT    B     XIT                                                              
         DROP  R4                                                               
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN (FOR SEND)                                      
***********************************************************************         
DR       DS    0H                                                               
         L     R6,AIO                                                           
         LA    R6,DMTHFRST-DMTHKEY(R6)                                          
         USING DMTHELD,R6                                                       
         XC    LSMDEST,LSMDEST                                                  
         MVC   LSMDEST,=C'STA'                                                  
         CLI   DMTHDEST,C'R'                                                    
         BNE   DR04                                                             
         MVC   LSMDEST,=C'REP'                                                  
DR04     OI    LSMDESTH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
DR05     XC    LSMMTHD,LSMMTHD                                                  
         XC    LSMBDEC,LSMBDEC                                                  
         CLI   DMTHMTHD,C'I'       WAS THE METHOD INBOX?                        
         BNE   DR10                 NO SKIP TO NEXT METHOD                      
         MVC   LSMMTHD,=C'INBOX'                                                
         B     DR25                                                             
*                                                                               
DR10     CLI   DMTHMTHD,C'F'       WAS THE METHOD FAX?                          
         BNE   DR20                 NO SKIP TO NEXT METHOD                      
         MVC   LSMMTHD(3),=C'FAX'                                               
         B     DR25                                                             
*                                                                               
DR20     MVC   LSMMTHD,=C'EMAIL'   THE METHOD WAS EMAIL                         
         MVC   LSMBDEC,DMTHBDEC    BDE COMMON NAME                              
*                                                                               
DR25     OI    LSMMTHDH+6,X'80'    TRANSMIT FIELDS                              
         OI    LSMBDECH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR30     CLI   CALLSTCK,X'F9'      DO WE UNPROTECT THE FIELDS?                  
         BE    *+12                                                             
         CLI   CALLSTCK,X'EA'      IF WE'RE COMING FROM  ORDER/SEND             
         BNE   DR40                   OR  MARKET/LIST  THEN WE CAN              
         NI    LSMDESTH+1,X'FF'-X'20'                                           
*                                                                               
DR40     XC    LSMSNAM,LSMSNAM     CLEAR SALESPERSON NAME                       
         OI    LSMSNAMH+6,X'80'                                                 
         XC    LSMFAX,LSMFAX       CLEAR FAX NUMBER                             
         OI    LSMFAXH+6,X'80'                                                  
         XC    LSMEDAT,LSMEDAT     CLEAR EFFECTIVE DATE                         
         OI    LSMEDATH+6,X'80'                                                 
*                                                                               
         CLI   TWAOFFC,C'*'        IS THIS A DDS TERMINAL?                      
         BE    DR41                YES, SHOW FAX OR SALESPERSON                 
         OI    LSMSHEDH+1,X'0C'    LOW INTENSITY                                
         OI    LSMSHEDH+6,X'80'    TRANSMIT                                     
         OI    LSMFHEDH+1,X'0C'    LOW INTENSITY                                
         OI    LSMFHEDH+6,X'80'    TRANSMIT                                     
         OI    LSMDHEDH+1,X'0C'    LOW INTENSITY                                
         OI    LSMDHEDH+6,X'80'    TRANSMIT                                     
         B     DR45                                                             
*                                                                               
DR41     L     R6,AIO                                                           
         MVI   ELCODE,DMFXELQ      FAX OVERIDE ELEMENT                          
         BAS   RE,GETEL            HAVE ONE?                                    
         BNE   DR42                NOPE                                         
         USING DMFXELD,R6                                                       
         MVC   LSMFAX,DMFXOVRD     FAX NUMBER                                   
         OI    LSMFAXH+6,X'80'     TRANSMIT                                     
         NI    LSMFHEDH+1,X'FF'-X'0C' NORMAL INTENSITY                          
         OI    LSMFHEDH+6,X'80'    TRANSMIT                                     
         OC    DMFXDATE,DMFXDATE                                                
         BZ    DR42                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(6,DMFXDATE),(5,LSMEDAT)   FAX DATE                  
*                                                                               
         DROP  R6                                                               
         OI    LSMEDATH+6,X'80'     TRANSMIT                                    
         NI    LSMDHEDH+1,X'FF'-X'0C' NORMAL INTENSITY                          
         OI    LSMDHEDH+6,X'80'    TRANSMIT                                     
*                                                                               
DR42     L     R6,AIO                                                           
         MVI   ELCODE,DMSPELQ      SALESPERSON ELEMENT                          
         BAS   RE,GETEL            HAVE ONE?                                    
         BNE   DR45                NOPE                                         
         USING DMSPELD,R6                                                       
         MVC   LSMSNAM,DMSPNAME    SALESPERSON NAME                             
         DROP  R6                                                               
         OI    LSMSNAMH+6,X'80'    TRANSMIT                                     
         NI    LSMSHEDH+1,X'FF'-X'0C' NORMAL INTENSITY                          
         OI    LSMSHEDH+6,X'80'    TRANSMIT                                     
*                                                                               
DR45     XC    LSMCDTE,LSMCDTE                                                  
         XC    LSMCTIM,LSMCTIM                                                  
         XC    LSMADTE,LSMADTE                                                  
         XC    LSMATIM,LSMATIM                                                  
*                                                                               
         L     R6,AIO              *** THIS IS THE ENTRY FOR XREC ***           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL            MUST BE THERE                                
         BNE   DR90                END IT IF NOT THERE, FOR NOW                 
         USING ACTVD,R6                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(5,LSMCDTE)   ADDED DATE                
*                                                                               
         OC    ACTVCHDT,ACTVCHDT   HAS RECORD BEEN CHANGED BEFORE?              
         BZ    DR50                 NO, SKIP NEXT DATCON CALL                   
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(5,LSMADTE)   CHANGED DATE              
         DROP  R6                                                               
*                                                                               
DR50     L     R6,AIO                                                           
         MVI   ELCODE,DATTIMLQ     X'D1' - DATE AND TIME STAMP ELEM             
         BAS   RE,GETEL            MUST BE THERE                                
         BNE   DR90                END IT IF NOT THERE, FOR NOW                 
         USING DATTIMD,R6                                                       
         OC    DATTMCTM,DATTMCTM   ANY CREATION TIME?                           
         BZ    DR60                                                             
         GOTO1 HEXOUT,DMCB,DATTMCTM,LSMCTIM+1,L'DATTMCTM                        
         MVC   LSMCTIM(2),LSMCTIM+1                                             
         MVI   LSMCTIM+2,C'.'                                                   
*                                                                               
DR60     OC    DATTMGTM,DATTMGTM   ANY LAST UPDATE TIME?                        
         BZ    DR90                                                             
         GOTO1 HEXOUT,DMCB,DATTMGTM,LSMATIM+1,L'DATTMGTM                        
         MVC   LSMATIM(2),LSMATIM+1                                             
         MVI   LSMATIM+2,C'.'                                                   
         DROP  R6                                                               
*                                                                               
DR90     OI    LSMCDTEH+6,X'80'                                                 
         OI    LSMCTIMH+6,X'80'                                                 
         OI    LSMADTEH+6,X'80'                                                 
         OI    LSMATIMH+6,X'80'                                                 
*                                                                               
DRXIT    B     XIT                                                              
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN (FOR SEND)                                      
***********************************************************************         
VR       DS    0H                                                               
         CLI   CALLSTCK,X'F9'      SHOULD NOT BE HERE VALIDATING IF NOT         
         BE    *+12                  COMING FROM  ORDER/SEND  OR                
         CLI   CALLSTCK,X'EA'        MARKET/LIST  SCREENS                       
         BNE   DR                  GO RE-DISPLAY THE RECORD                     
*                                                                               
VRXIT    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LST      DS    0H                                                               
*                                                                               
         MVI   NLISTS,16           # OF LINES TO LIST, DEFAULT = 15             
         MVC   LLIST,=Y(DISNEXTL-DISEVTH)                                       
*                                                                               
         LA    R2,LSLSEL1H                                                      
         USING DISLINED,R2                                                      
         TWAXC LSLSEL1H,LSLEVTLH,PROT=Y  CLEAR ALL THE LINES                    
*                                                                               
         TM    MISCFLG1,MF1KYCHG         DID THE KEY CHANGE?                    
         BO    LST05                     YES: START FROM BEGINNING              
         TM    BITFLAG,BFPREVKY          DO WE HAVE A PREV KEY?                 
         BZ    LST05                     START FROM BEGINNING                   
         MVC   KEY,PREVKEY                                                      
         NI    BITFLAG,X'FF'-BFPREVKY                                           
         B     LST15                                                            
*                                                                               
LST05    LA    R4,KEY                                                           
         USING DMTHKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   DMTHTYPE,DMTHTYPQ          X'0D'                                 
         MVI   DMTHSBTY,DMTHSBTQ          X'3E'                                 
         MVC   DMTHAGMD,BAGYMD                                                  
         MVC   DMTHBYR,SVBUYER                                                  
         MVC   DMTHSTA,SVSTA                                                    
         MVC   DMTHCLT,SVCLT                                                    
         B     LST15                                                            
*                                                                               
LST15    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
LST30    CLC   KEY(DMTHSTA-DMTHKEY),KEYSAVE   CHECK UPTO BUYER                  
         BNE   LSTXIT                                                           
*                                                                               
* DO I HAVE STATION FILTER?                                                     
         TM    MISCFLG2,MF2STA                                                  
         BZ    LST35                                                            
         CLC   DMTHSTA,SVSTA                                                    
         BNE   LSTXIT                                                           
*                                                                               
* DO I HAVE CLIENT FILTER?                                                      
LST35    TM    MISCFLG2,MF2CLT                                                  
         BZ    LST40                                                            
         CLC   DMTHCLT,SVCLT                                                    
         BE    LST40                                                            
         TM    MISCFLG2,MF2STA                                                  
         BNO   LSTNEXT                                                          
         B     LSTXIT                                                           
         DROP  R4                                                               
*                                                                               
LST40    DS    0H                                                               
         LA    R0,LSLPFLNH                                                      
         CR    R2,R0                                                            
         BNL   LSTXIT10                                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING DMTHKEY,R6                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+2(L'DMTHSTA),DMTHSTA                                        
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,WORK+9                                   
         MVC   DISSTA,WORK+9                                                    
*                                                                               
         OC    DMTHCLT,DMTHCLT                                                  
         BZ    LST45                                                            
*                                                                               
         MVC   SAVEKEY2(13),KEY                                                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),DMTHCLT                                                 
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING CLTHDRD,R4                                                       
         L     R4,AIO                                                           
         GOTO1 CLUNPK,DMCB,(CPROF+6,DMTHCLT),DISCLT   SHOW CLIENT               
         DROP  R4                                                               
*                                                                               
         LA    R4,KEY                                                           
         MVC   KEY(13),SAVEKEY2                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(DMTHSTA-DMTHKEY),KEYSAVE   CHECK UP TO BUYER                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
*                                                                               
LST45    MVI   ELCODE,DMTHELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LST50                                                            
         USING DMTHELD,R6                                                       
         MVC   DISLDEST,=C'REP'                                                 
         CLI   DMTHDEST,C'S'                                                    
         BNE   *+10                                                             
         MVC   DISLDEST,=C'STA'                                                 
*                                                                               
         CLI   DMTHMTHD,C'I'       WAS THE METHOD INBOX?                        
         BNE   LST45A               NO SKIP TO NEXT METHOD                      
         MVC   DISMTHD,=C'INBOX'                                                
         B     LST50                                                            
*                                                                               
LST45A   CLI   DMTHMTHD,C'F'       WAS THE METHOD FAX?                          
         BNE   LST45B               NO SKIP TO NEXT METHOD                      
         MVC   DISMTHD(3),=C'FAX'                                               
         B     LST50                                                            
*                                                                               
LST45B   MVC   DISMTHD,=C'EMAIL'   THE METHOD WAS EMAIL                         
         MVC   DISSLNAM,DMTHBDEC                                                
         DROP  R6                                                               
*                                                                               
LST50    GOTO1 LISTMON                                                          
*                                                                               
         LA    R2,DISNEXTL-DISLINED(R2)                                         
*                                                                               
LSTNEXT  GOTO1 SEQ                                                              
         B     LST30                                                            
         DROP  R2                                                               
*                                                                               
LSTXIT10 MVC   PREVKEY,KEY                                                      
         OI    BITFLAG,BFPREVKY                                                 
*                                                                               
LSTXIT   B     XIT                                                              
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
*                                                                               
         LA    R2,SPFTABLE         USE PREDEFINED PFKEYS                        
*                                                                               
STPFINIT GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
STPFX    CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+10                NO                                           
         MVC   LSMPFLN(11),=CL11'PF12=Return'                                   
         OI    LSMPFLNH+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETMFKYS NTR1                                                                   
         LA    R2,MPFTABLE         USE PREDEFINED PFKEYS                        
*                                                                               
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
         XC    LSLPFLN,LSLPFLN                                                  
         MVC   LSLPFLN(11),=C'PF2=Display'                                      
         OI    LSLPFLNH+6,X'80'                                                 
*                                                                               
         CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+10                NO                                           
         MVC   LSLPFLN+13(9),=CL9'12=Return'                                    
         OI    LSLPFLNH+6,X'80'                                                 
*                                                                               
         CLI   PFKEY,0                                                          
         BE    STMFX                                                            
         CLI   PFKEY,12                                                         
         BE    STMFX                                                            
         ZIC   R0,PFKEY                                                         
         AHI   R0,12                                                            
         STC   R0,PFKEY                                                         
*                                                                               
         LH    R3,CURDISP                                                       
         AR    R3,RA                                                            
         USING DISLINED,R3                SET FIELDS FOR SEND                   
         XC    XMITCLT,XMITCLT                                                  
         CLC   DISCLT,=C'ALL'                                                   
         BE    *+10                                                             
         MVC   XMITCLT,DISCLT                                                   
         OC    XMITCLT,SPACES                                                   
*                                                                               
         LA    R2,MPFTABLE         YES, USE LIST MPFKEY TABLE                   
         OI    CTLRFLG1,CF1TSELQ                                                
         GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
STMFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLDATE MVI   GERROR1,INVDATE                                                  
         B     ERREXIT                                                          
*                                                                               
INVLDEST MVI   GERROR1,INVDEST                                                  
         B     ERREXIT                                                          
*                                                                               
RECXISTS MVI   GERROR1,RECEXIST    RECORD ALREADY EXISTS                        
         B     ERREXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND    RECORD NOT FOUND                             
         B     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 2)                                                     
***********************************************************************         
ER2EXIT  MVI   GETMSYS,2           SPOT MESSAGES                                
         B     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
***********************************************************************         
* ERRORS WITH REPLACEMENT TEXT (&1)                                             
***********************************************************************         
BYRALRDY MVI   GERROR1,GOTBYRAL    BUYER &1 ALREADY ASSIGNED TO THIS ..         
         B     ERRRTEXT                                                         
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         B     INFEXIT                                                          
*                                                                               
NTRCHNGS MVI   GERROR1,RCDSPCHA    RECORD DISPLAYED - NOW ENTER CHANGES         
         B     INFEXIT                                                          
*                                                                               
RECCHNGD MVI   GERROR1,RCWASCHA    RECORD WAS CHANGED - ENTER NEXT RE..         
         B     INFEXIT                                                          
*                                                                               
MAKESELS MVI   GERROR1,LSTDISPL    LIST DISPLAYED - SELECT OR HIT ENT..         
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
***********************************************************************         
* INFO MESSAGES (SYSTEM 23)                                                     
***********************************************************************         
ONITSWAY MVI   GERROR1,ODRONWAY    ** DARE ORDER &1 IS ON ITS WAY! **           
         B     INFRTEXT                                                         
*                                                                               
OURMESGE MVI   GERROR1,INFOMESS    &1                                           
*                                                                               
INFRTEXT LA    R1,MYINFXIT                                                      
         B     *+8                                                              
ERRRTEXT LA    R1,ERREXIT                                                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,23                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         DROP  RF                                                               
         BR    R1                                                               
*                                                                               
NOTHING  DC    H'0'                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
HEADING  SSPEC H2,55,RUN                                                        
         DC    X'00'                                                            
*                                                                               
ZEROS    DC    12C'0'                                                           
RELO     DS    A                                                                
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS                                                       
***********************************************************************         
MPFTABLE  DS    0C                                                              
*                                                                               
         DC    AL1(MPF02X-*,02,0,0,0,PFTRETRN)                                  
         DC    CL3'S',CL8' ',CL8' '                                             
MPF02X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
*                                                                               
* ACTUAL LAST METHOD STATUS                                                     
*                                                                               
         DC    AL1(MPF14X-*,14,PFTCPROG,0,(MPF14X-MPF14)/KEYLNQ,0)              
         DC    CL3' ',CL8'LSTMTHD',CL8'DISPLAY'                                 
MPF14    DC    AL1(KEYTYTWA,L'LSLMED-1),AL2(LSLMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'LSLBYR-1),AL2(LSLBYR-T234FFD)                     
         DC    AL1(KEYTYCUR,L'DISSTA-1),AL2(0)                                  
         DC    AL1(KEYTYWS,L'XMITCLT-1),AL2(XMITCLT-MYAREAD)                    
MPF14X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
*                                                                               
SPFTABLE  DS    0C                                                              
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE SPOMSERROR                                                     
         EJECT                                                                  
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDCOMFACSD                                                                    
* DMPRTQL                                                                       
* FAFACTS                                                                       
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* CTGENEDICT                                                                    
* FATIOB                                                                        
* SPGENAGY                                                                      
* SPGENCLT                                                                      
* SPGENEST                                                                      
* SPGENMKT                                                                      
* SPGENPRD                                                                      
* SPADAVCOM                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBIGBOX                                                       
BOXDL    EQU   *-BOXD                                                           
       ++INCLUDE DDWIDED                                                        
WIDEDL   EQU   *-WIDED                                                          
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDSPOOK           SOON JOB PARAMETER BLOCK                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTGENEDICT                                                     
*PREFIX=CT$                                                                     
       ++INCLUDE CTGENSTAD                                                      
*PREFIX=                                                                        
       ++INCLUDE CTGENAGRD                                                      
       ++INCLUDE SPAUTHD                                                        
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPADAVCOM                                                      
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSC6D          (OUR DISPLAY SCREEN)                         
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSCAD          (OUR LIST SCREEN)                            
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPOMSWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRMTH                                                     
         EJECT                                                                  
       ++INCLUDE DDACTIVD          ACTIVITY ELEMENT DSECT                       
         EJECT                                                                  
***********************************************************************         
* MY STORAGE AREA                                                               
***********************************************************************         
MYAREAD  DSECT                                                                  
BITFLAG  DS    XL1                 VARIOUS FLAGS                                
BFPREVKY EQU   X'80'               PREVIOUS KEY TO BE USED                      
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS SET 1                    
MF1KYCHG EQU   X'80'                 A KEY FIELD HAS BEEN CHANGED               
*                                                                               
MISCFLG2 DS    XL1                 MISCELLANEOUS FLAGS SET 2                    
MF2STA   EQU   X'80'               FILTERING ON STATION                         
MF2CLT   EQU   X'40'               FILTERING ON CLIENT                          
*                                                                               
SAVEKEY  DS    CL(L'DMTHKEY)                                                    
PREVKEY  DS    CL(L'DMTHKEY)                                                    
SVBUYER  DS    CL3                                                              
SVSTA    DS    XL3                                                              
SVCLT    DS    CL2                                                              
*                                                                               
PROFDAR  DS    CL16                DARE PROFILE                                 
PDARDEMS EQU   PROFDAR+1             INCLUDE DEMOS IN ORDER?                    
PDARBCOM EQU   PROFDAR+2             INCLUDE BUY COMMENTS IN ORDER?             
PDARTSRP EQU   PROFDAR+6   3 BYTES   TRADE SPECIAL REP DIGITS                   
*                                                                               
FAKEFLDH DS    CL8                 FAKE FIELD HEADER AND FIELD DATA             
FAKEFLD  DS    CL60                                                             
*                                                                               
XMITCLT  DS    CL3                                                              
*                                                                               
SVSECAGY DS    XL2                 SECURITY AGENCY                              
*                                                                               
NLISTQ   EQU   (LSLPFLNH-LSLSEL1H)/DISLNQ                                       
SAVEKEY2 DS    CL(L'DMTHKEY)                                                    
         EJECT                                                                  
***********************************************************************         
* DISPLAY LINE DSECT                                                            
***********************************************************************         
DISLINED DSECT                                                                  
DISSELH  DS    CL(L'LSLSEL1H)                                                   
DISSEL   DS    CL(L'LSLSEL1)                                                    
DISEVTH  DS    CL8                                                              
DISSTA   DS    CL8                 STATION                                      
         DS    CL2                                                              
DISCLT   DS    CL3                 CLIENT                                       
         DS    CL2                                                              
DISLDEST DS    CL3                 DESTINATION                                  
         DS    CL3                                                              
DISMTHD  DS    CL5                 METHOD                                       
         DS    CL2                                                              
DISSLNAM DS    CL40                GROUP NAME                                   
         DS    CL6                                                              
DISNEXTL DS    0C                                                               
DISLNQ   EQU   *-DISLINED                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPOMS16   01/16/07'                                      
         END                                                                    
