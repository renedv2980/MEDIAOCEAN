*          DATA SET ACPRO3C    AT LEVEL 013 AS OF 04/03/08                      
*PHASE T60B3CA                                                                  
         TITLE 'T60B3C - STUDIO TYPE MAINT'                                     
T60B3C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T60B3C**,RA,CLEAR=YES                               
         LR    R4,RC                                                            
         USING MYD,R4                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE8    GOTO1 CANWEDEL                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
VKEY     NTR1                      STUDIO TYPE RECORD                           
         LA    R2,PROSTUH                                                       
         GOTO1 ANY                                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),4                                                          
         BH    ERREND                                                           
*                                                                               
         USING STURECD,R6                                                       
         LA    R6,KEY              FORMAT KEY OF STUDIO TYPE RECORD             
         XC    STUKEY,STUKEY                                                    
         MVI   STUKTYP,STUKTYPQ                                                 
         MVI   STUKSUB,STUKSUBQ                                                 
         MVC   STUKCPY,CUL                                                      
         MVC   STUKCODE,WORK                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY KEY                                                      
*                                                                               
DKEY     NTR1                                                                   
         L     R6,AIO                                                           
         MVC   PROSTU,3(R6)                                                     
         LA    R2,PROSTUH                                                       
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
DREC     NTR1                                                                   
         GOTO1 VCLEARF,DMCB,PROOFFH,PROLAST                                     
*                                                                               
         LA    R2,PRODESCH                                                      
         GOTO1 NAMEOUT                                                          
*                                                                               
         GOTO1 PERSOUT                                                          
         MVC   PROLACT,SPACES                                                   
         MVC   PROLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PROLACT+15(20),WORK+20                                           
         OI    PROLACTH+6,X'80'                                                 
*                                                                               
         USING STUELD,R5                                                        
         MVI   ELCODE,STUELQ                                                    
         BAS   RE,GETELIO                                                       
         LR    R5,R6                                                            
         MVC   PROMED,SPACES       OPTIONAL MEDIA CODE                          
         MVC   PROMEDN,SPACES                                                   
         OI    PROMEDH+6,X'80'                                                  
         OI    PROMEDNH+6,X'80'                                                 
*                                                                               
         MVC   AIO,AIO2            SAVE AIO AND KEY                             
         MVC   SAVEKEY1,KEY                                                     
*                                                                               
         CLI   STUMED,X'41'                                                     
         BL    DREC02                                                           
         LA    R2,PROMEDH                                                       
         MVC   8(1,R2),STUMED      IF THERE, SHOW IT                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),CUL                                                     
         MVC   KEY+2(1),STUMED                                                  
         GOTO1 READ                                                             
         MVI   ELCODE,ACMDELQ                                                   
         BAS   RE,GETELIO                                                       
         USING ACMEDIAD,R6                                                      
         LA    R2,PROMEDNH                                                      
         MVC   8(L'ACMDDESC,R2),ACMDDESC                                        
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
DREC02   LA    R2,PROVENH          VENDOR A/C                                   
         OI    6(R2),X'80'                                                      
         MVC   8(15,R2),STUVEND+1                                               
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),STUVEND                                                  
         GOTO1 READ                                                             
         LA    R2,PROVENNH                                                      
         GOTO1 NAMEOUT                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,STULN            GET LENGTH OF ELEMENT                        
         SH    R1,=Y(STUOFF-STUELD)                                             
         BZ    DRECX                                                            
*                                                                               
         USING LND,R2                                                           
         LA    R2,PROOFFH          OFFICE ON SCREEN                             
         LA    R5,STUOFF           OFFICE IN ELEMENT                            
*                                                                               
DREC04   MVC   LNDOFF,0(R5)                                                     
         OI    LNDOFFH+6,X'80'                                                  
         MVC   LNDCLI,2(R5)                                                     
         OI    LNDCLIH+6,X'80'                                                  
         LA    R5,L'STUOCLN(R5)                                                 
         LA    R2,LNDLNG(R2)                                                    
         SH    R1,=Y(L'STUOCLN)                                                 
         BNZ   DREC04                                                           
*                                                                               
DRECX    MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
*              VALIDATE RECORD                                                  
*                                                                               
VREC     NTR1                                                                   
         MVI   ELCODE,STUELQ       DELETE DATA ELEMENT                          
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,PRODESCH                                                      
         GOTO1 NAMEIN                                                           
*                                                                               
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
         MVC   AIO,AIO2            SAVE AIO AND KEY                             
         MVC   SAVEKEY1,KEY                                                     
*                                                                               
         USING STUELD,R6                                                        
         MVI   STUEL,STUELQ                                                     
         MVI   STUMED,0                                                         
         LA    R2,PROMEDH          MEDIA IS OPTIONAL                            
         CLI   5(R2),0                                                          
         BE    VREC02                                                           
*                                                                               
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALMED                                                           
         MVI   OPTION,0                                                         
         MVC   STUMED,PROMED                                                    
*                                                                               
VREC02   LA    R2,PROVENH          VENDOR IS REQUIRED                           
         GOTO1 ANY                                                              
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),CUL                                                       
*                                                                               
         LA    R1,UNITLST          VALIDATE UNIT/LEDGER                         
*                                                                               
VREC04   CLC   0(2,R1),WORK                                                     
         BE    VREC06                                                           
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   VREC04                                                           
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
*                                                                               
VREC06   MVC   KEY+1(14),WORK                                                   
         GOTO1 READ                                                             
         BAS   RE,BALEL                                                         
         MVC   STUVEND,KEY         MOVE RECORD KEY TO ELEMENT                   
*                                                                               
         LA    R2,PROVENNH                                                      
         GOTO1 NAMEOUT                                                          
*                                                                               
         USING LND,R5                                                           
         LA    R0,MAXNUM           MAXIMUM NUMBER ON SCREEN ENTRIES             
         LA    R5,PROOFFH          GET FIRST OFFICE ON SCREEN                   
         LA    R6,STUOFF           GET ADDRESS OF FIRST OFFICE                  
*                                                                               
VOFF     LA    R3,OFFTAB                                                        
*                                                                               
VOFF02   OC    LNDOFF,SPACES       FILL IT WITH BKANKS                          
         CLI   LNDOFFH+5,X'00'     ANY OFFICE CODE?                             
         BNE   *+12                YES                                          
         CLI   LNDCLIH+5,X'00'     NO, ANY CLIENT?                              
         BE    VOFF12              NO, LOOK AT NEXT ONE                         
*                                                                               
VOFF04   LA    R2,LNDCLIH          HAVE OFFICE, MUST HAVE CLIENT                
         CLI   LNDCLIH+5,X'00'                                                  
         BE    VOFF16                                                           
         OC    0(5,R3),0(R3)       ANY ENTRIES YET?                             
         BZ    VOFF08              NO, VALIDATE OFFICE                          
         CLC   0(2,R3),LNDOFF      DO WE HAVE THIS OFFICE ALREADY?              
         BNE   VOFF06              NO                                           
         CLC   2(3,R3),LNDCLI      YES, DO WE HAVE THIS CLIENT?                 
         BNE   VOFF06              NO                                           
         LA    R2,LNDOFFH          YES, ERROR                                   
         MVI   ERROR,DUPINPUT                                                   
         B     ERREND                                                           
*                                                                               
VOFF06   LA    R3,OFFTABL(R3)      GET NEXT TABLE ENTRY                         
         B     VOFF04                                                           
*                                                                               
VOFF08   TM    COMPSTA1,X'20'      VALIDATE OFFICES ?                           
         BZ    VOFF10              NO                                           
         CLI   LNDOFFH+5,X'00'     IS THERE AN OFFICE ?                         
         BE    VOFF10              NO, CHECK THE CLIENT                         
*                                                                               
         LA    R2,LNDOFFH                                                       
         GOTO1 VALOFF              YES                                          
         BAS   RE,SETGET           SETUP AND GET OPTIONS                        
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVI   ERROR,ISSTUD                                                     
         CLI   GOSTUDIO,C'Y'       IS THIS A STUDIO OFFICE ?                    
         BE    ERREND              YES, ERROR                                   
*                                                                               
VOFF10   LA    R2,LNDCLIH                                                       
         GOTO1 VALCLI                                                           
         BAS   RE,SETGET           SETUP AND GET OPTIONS                        
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         MVI   ERROR,NOTSTUD                                                    
         CLI   GOSTUDIO,C'Y'       IS THIS CLIENT IN A STUDIO OFFICE?           
         BNE   ERREND              NO, ERROR                                    
         MVC   0(2,R3),LNDOFF      SAVE OFFICE AND CLIENT                       
         OC    0(2,R3),SPACES                                                   
         MVC   2(3,R3),CLICODE                                                  
*                                                                               
VOFF12   LA    R5,LNDLNG(R5)       GET NEXT SCREEN FIELD                        
         BCT   R0,VOFF                                                          
*                                                                               
VOFF14   LA    R0,MAXNUM                                                        
         LA    R3,OFFTAB                                                        
         OC    0(5,R3),0(R3)       ANY ENTRIES?                                 
         BNZ   VOFF18              YES                                          
         LA    R2,PROOFFH          NO, ERROR                                    
*                                                                               
VOFF16   MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
*                                                                               
VOFF18   OC    0(5,R3),0(R3)       ANY (MORE) ENTRIES?                          
         BZ    VOFF20              NO, DONE                                     
         MVC   0(L'STUOFF,R6),0(R3)                                             
         MVC   2(L'STUCLN,R6),2(R3)                                             
         LA    R6,L'STUOCLN(R6)                                                 
         LA    R3,OFFTABL(R3)                                                   
         BCT   R0,VOFF18                                                        
*                                                                               
VOFF20   MVC   AIO,AIO1                                                         
         MVC   KEY,SAVEKEY1                                                     
         LR    R0,R6                                                            
         LA    R6,ELEMENT                                                       
         SR    R0,R6                                                            
         STC   R0,STULN                                                         
         GOTO1 ADDELEM                                                          
         GOTO1 PERSIN                                                           
         DROP  R5,R6                                                            
         B     XIT                                                              
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
*                                                                               
SETGET   NTR1                                                                   
         MVC   GOADM,DATAMGR                                                    
         LA    R1,GOXBLOCK                                                      
         ST    R1,GOAEXT                                                        
         XC    GOACLI,GOACLI                                                    
         XC    GOAPRO,GOAPRO                                                    
         XC    GOAJOB,GOAJOB                                                    
         XC    GOACOMP,GOACOMP                                                  
         XC    GOSELOG,GOSELOG                                                  
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELOFC,EFFOFFC                                                 
         MVI   GOWHICH,0                                                        
         MVI   GOSELLEV,0                                                       
         MVC   GOEFFOG,EFFOFG                                                   
         MVC   GOEFFOFC,EFFOFFC                                                 
         B     XIT                                                              
*                                                                               
BALEL    NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,ACBLELQ      MUST HAVE BALANCE ELEMENT                    
         MVI   ERROR,INVPOST                                                    
         BAS   RE,GETELIO                                                       
         BNE   ERREND                                                           
         B     XIT                                                              
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
UNITLST  DC    C'SVSRSBSWSXSYSC',X'FF'                                          
*                                                                               
MYEND    MVI   ERROR,SUPPLIED      USING MY OWN ERROR MSG                       
         B     ERREND                                                           
*                                                                               
INVEND   MVI   ERROR,INVALID                                                    
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYD      DSECT                                                                  
OFFTAB   DS    CL(OFFTABL*MAXNUM)  TABLE OF OFFICES/CLIENTS                     
OFFTABL  EQU   L'STUOCLN           LENGTH OF EACH ENTRY                         
MAXNUM   EQU   46                  MAXIMUM NUMBER OF ENTRIES                    
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE ACGOXBLOCK                                                     
MYDEND   EQU   *                                                                
         EJECT                                                                  
LND      DSECT                                                                  
LNDOFFH  DS    CL(L'PROOFFH)                                                    
LNDOFF   DS    CL(L'PROOFF)                                                     
LNDOFX   DS    CL(L'PROOFFX)                                                    
LNDCLIH  DS    CL(L'PROCLNH)                                                    
LNDCLI   DS    CL(L'PROCLN)                                                     
LNDCLIX  DS    CL(L'PROCLNX)                                                    
LNDLNG   EQU   *-LND                                                            
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROCCD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACPRO3C   04/03/08'                                      
         END                                                                    
